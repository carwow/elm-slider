module DoubleSlider exposing (Model, Msg, update, subscriptions, view, fallbackView, formatCurrentRange, defaultModel)

{-| A single slider built natively in Elm


# Model

@docs Model, defaultModel


# Update

@docs Msg, update, subscriptions


# View

@docs view, fallbackView, formatCurrentRange

-}

import Browser
import Browser.Events
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode exposing (map)
import DOM exposing (boundingClientRect)
import Mouse exposing (..)


{-| The base model for the slider
-}
type alias Model =
    { min : Float
    , max : Float
    , step : Int
    , lowValue : Float
    , highValue : Float
    , dragging : Bool
    , draggedValueType : SliderValueType
    , rangeStartValue : Float
    , thumbStartingPosition : Float
    , dragStartPosition : Float
    , thumbParentWidth : Float
    , overlapThreshold : Float
    , minFormatter : Float -> String
    , maxFormatter : Float -> String
    , currentRangeFormatter : Float -> Float -> Float -> Float -> String
    }


type SliderValueType
    = LowValue
    | HighValue
    | None


{-| The basic type accepted by the update
-}
type Msg
    = TrackClicked SliderValueType String
    | DragStart SliderValueType Position Float Float
    | DragAt Position
    | DragEnd Position
    | RangeChanged SliderValueType String Bool


{-| Returns a default range slider
-}
defaultModel : Model
defaultModel =
    { min = 0
    , max = 100
    , step = 10
    , lowValue = 0
    , highValue = 100
    , overlapThreshold = 1
    , dragging = False
    , draggedValueType = None
    , rangeStartValue = 0
    , thumbStartingPosition = 0
    , thumbParentWidth = 0
    , dragStartPosition = 0
    , minFormatter = toString
    , maxFormatter = toString
    , currentRangeFormatter = defaultCurrentRangeFormatter
    }


defaultCurrentRangeFormatter : Float -> Float -> Float -> Float -> String
defaultCurrentRangeFormatter lowValue highValue min max =
    String.join " " [ (toString lowValue), "-", (toString highValue) ]


{-| takes a model and a message and applies it to create an updated model
-}
update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update message model =
    case message of
        RangeChanged valueType newValue shouldFetchModels ->
            let
                convertedValue =
                    String.toFloat newValue |> Maybe.withDefault 0

                newModel =
                    case valueType of
                        LowValue ->
                            { model | lowValue = convertedValue }

                        HighValue ->
                            { model | highValue = convertedValue }

                        None ->
                            model
            in
                ( newModel, Cmd.none, shouldFetchModels )

        TrackClicked valueType newValue ->
            let
                convertedValue =
                    snapValue (String.toFloat newValue |> Maybe.withDefault 0) model.step

                newModel =
                    case valueType of
                        LowValue ->
                            { model | lowValue = convertedValue }

                        HighValue ->
                            { model | highValue = convertedValue }

                        None ->
                            model
            in
                ( newModel, Cmd.none, True )

        DragStart valueType position offsetLeft offsetWidth ->
            let
                newModel =
                    { model
                        | dragging = True
                        , draggedValueType = valueType
                        , rangeStartValue =
                            case valueType of
                                LowValue ->
                                    model.lowValue - model.min

                                HighValue ->
                                    model.highValue - model.min

                                None ->
                                    0
                        , thumbStartingPosition = offsetLeft + 16
                        , thumbParentWidth = offsetWidth
                        , dragStartPosition = (toFloat position.x)
                    }
            in
                ( newModel, Cmd.none, False )

        DragAt position ->
            let
                rangeStart =
                    case model.draggedValueType of
                        HighValue ->
                            model.rangeStartValue

                        LowValue ->
                            model.max - model.rangeStartValue - model.min

                        None ->
                            0

                offset =
                    case model.draggedValueType of
                        HighValue ->
                            model.thumbStartingPosition

                        LowValue ->
                            model.thumbParentWidth - model.thumbStartingPosition

                        None ->
                            0

                ratio =
                    rangeStart / offset

                delta =
                    ((toFloat position.x) - model.dragStartPosition)

                newValue =
                    case model.draggedValueType of
                        HighValue ->
                            model.min + snapValue ((offset + delta) * ratio) model.step

                        LowValue ->
                            model.min + snapValue ((model.thumbParentWidth - offset + delta) * ratio) model.step

                        None ->
                            0

                newModel =
                    if (model.draggedValueType == LowValue && newValue + ((toFloat model.step) * model.overlapThreshold) > model.highValue) then
                        model
                    else if (model.draggedValueType == HighValue && newValue - ((toFloat model.step) * model.overlapThreshold) < model.lowValue) then
                        model
                    else if newValue >= model.min && newValue <= model.max then
                        case model.draggedValueType of
                            LowValue ->
                                { model | lowValue = newValue }

                            HighValue ->
                                { model | highValue = newValue }

                            None ->
                                model
                    else
                        model
            in
                ( newModel, Cmd.none, False )

        DragEnd position ->
            ( { model
                | dragging = False
              }
            , Cmd.none
            , True
            )


snapValue : Float -> Int -> Float
snapValue value step =
    toFloat (((round value) // step) * step)


onOutsideRangeClick : Model -> Json.Decode.Decoder Msg
onOutsideRangeClick model =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            snapValue ((model.max / rectangle.width) * mouseX) model.step

                        valueType =
                            if newValue < model.lowValue then
                                LowValue
                            else
                                HighValue
                    in
                        valueType
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            (((model.max - model.min) / rectangle.width) * mouseX) + model.min
                    in
                        String.fromInt (round newValue)
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map2 TrackClicked valueTypeDecoder valueDecoder


onInsideRangeClick : Model -> Json.Decode.Decoder Msg
onInsideRangeClick model =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        centerThreshold =
                            rectangle.width / 2

                        valueType =
                            if mouseX < centerThreshold then
                                LowValue
                            else
                                HighValue
                    in
                        valueType
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            snapValue ((((model.highValue - model.lowValue) / rectangle.width) * mouseX) + model.lowValue) model.step
                    in
                        String.fromInt (round newValue)
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map2 TrackClicked valueTypeDecoder valueDecoder


onThumbMouseDown : SliderValueType -> Json.Decode.Decoder Msg
onThumbMouseDown valueType =
    Json.Decode.map4
        DragStart
        (Json.Decode.succeed valueType)
        positionDecoder
        (Json.Decode.at [ "target", "offsetLeft" ] Json.Decode.float)
        (Json.Decode.at [ "target", "offsetParent", "offsetWidth" ] Json.Decode.float)


onRangeChange : SliderValueType -> Bool -> Json.Decode.Decoder Msg
onRangeChange valueType shouldFetchModels =
    Json.Decode.map3
        RangeChanged
        (Json.Decode.succeed valueType)
        targetValue
        (Json.Decode.succeed shouldFetchModels)


{-| Displays the slider using two inputs
-}
fallbackView : Model -> Html Msg
fallbackView model =
    let
        lowValue =
            round model.lowValue

        highValue =
            round model.highValue

        progressRatio =
            100 / (model.max - model.min)

        progressLow =
            String.fromFloat ((model.lowValue - model.min) * progressRatio) ++ "%"

        progressHigh =
            String.fromFloat ((model.max - model.highValue) * progressRatio) ++ "%"
    in
        div []
            [ div
                [ Html.Attributes.class "input-range-container" ]
                [ Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min (String.fromFloat model.min)
                    , Html.Attributes.max (String.fromFloat model.max)
                    , Html.Attributes.value <| (String.fromFloat model.lowValue)
                    , Html.Attributes.step (String.fromInt model.step)
                    , Html.Attributes.class "input-range input-range--first"
                    , Html.Events.on "change" (onRangeChange LowValue True)
                    , Html.Events.on "input" (onRangeChange LowValue False)
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min (String.fromFloat model.min)
                    , Html.Attributes.max (String.fromFloat model.max)
                    , Html.Attributes.value <| (String.fromFloat model.highValue)
                    , Html.Attributes.step (String.fromInt model.step)
                    , Html.Attributes.class "input-range input-range--second"
                    , Html.Events.on "change" (onRangeChange HighValue True)
                    , Html.Events.on "input" (onRangeChange HighValue False)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__track"
                    , Html.Events.on "click" (onOutsideRangeClick model)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__progress"
                    , Html.Attributes.style "left" progressLow
                    , Html.Attributes.style "right" progressHigh
                    , Html.Events.on "click" (onInsideRangeClick model)
                    ]
                    []
                ]
            , div
                [ Html.Attributes.class "input-range-labels-container" ]
                [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.minFormatter model.min) ]
                , div
                    [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                    [ Html.text (formatCurrentRange model) ]
                , div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.maxFormatter model.max) ]
                ]
            ]


{-| Renders the current values using the formatter
-}
formatCurrentRange : Model -> String
formatCurrentRange model =
    if model.lowValue == model.min && model.highValue == model.max then
        ""
    else
        model.currentRangeFormatter model.lowValue model.highValue model.min model.max


{-| Displays the slider
-}
view : Model -> Html Msg
view model =
    let
        lowValue =
            round model.lowValue

        highValue =
            round model.highValue

        progressRatio =
            100 / (model.max - model.min)

        lowThumbStartingPosition =
            String.fromFloat ((model.lowValue - model.min) * progressRatio) ++ "%"

        highThumbStartingPosition =
            String.fromFloat ((model.highValue - model.min) * progressRatio) ++ "%"

        progressLow =
            String.fromFloat ((model.lowValue - model.min) * progressRatio) ++ "%"

        progressHigh =
            String.fromFloat ((model.max - model.highValue) * progressRatio) ++ "%"

        mouseDownEvent t =
            onThumbMouseDown t
                |> Json.Decode.map (\msg -> { message = msg, stopPropagation = True, preventDefault = True })
                |> Html.Events.custom "mousedown"
    in
        div []
            [ div
                [ Html.Attributes.class "input-range-container" ]
                [ div
                    [ Html.Attributes.class "slider-thumb slider-thumb--first"
                    , Html.Attributes.style "left" lowThumbStartingPosition
                    , Html.Attributes.style "float" "left"
                    , mouseDownEvent LowValue
                    ]
                    []
                , div
                    [ Html.Attributes.class "slider-thumb slider-thumb--second"
                    , Html.Attributes.style "left" highThumbStartingPosition
                    , Html.Attributes.style "float" "left"
                    , mouseDownEvent HighValue
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__track"
                    , Html.Events.on "click" (onOutsideRangeClick model)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__progress"
                    , Html.Attributes.style "left" progressLow
                    , Html.Attributes.style "right" progressHigh
                    , Html.Events.on "click" (onInsideRangeClick model)
                    ]
                    []
                ]
            , div
                [ Html.Attributes.class "input-range-labels-container" ]
                [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.minFormatter model.min) ]
                , div
                    [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                    [ Html.text (formatCurrentRange model) ]
                , div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.maxFormatter model.max) ]
                ]
            ]


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dragging then
        let
            moveDecoder : Json.Decode.Decoder Msg
            moveDecoder =
                Json.Decode.map DragAt positionDecoder

            upDecoder : Json.Decode.Decoder Msg
            upDecoder =
                Json.Decode.map DragEnd positionDecoder
        in
            Sub.batch
                [ Browser.Events.onMouseMove moveDecoder
                , Browser.Events.onMouseUp upDecoder
                ]
    else
        Sub.none
