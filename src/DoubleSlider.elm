module DoubleSlider exposing (Model, Msg, init, update, subscriptions, view, fallbackView, formatCurrentValue)

{-| A single slider built natively in Elm


# Model

@docs Model


# Update

@docs Msg, update, subscriptions


# Configuring the slider

@docs init


# View

@docs view, fallbackView, formatCurrentValue

-}

import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode exposing (map)
import DOM exposing (boundingClientRect)
import Mouse exposing (Position)


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
    , formatter : Float -> String
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
init : { min : Float, max : Float, step : Int, lowValue : Float, highValue : Float, overlapThreshold : Float, formatter : Float -> String, overMax : Bool } -> Model
init config =
    { min = config.min
    , max = config.max
    , step = config.step
    , lowValue = config.lowValue
    , highValue = config.highValue
    , overlapThreshold = config.overlapThreshold
    , dragging = False
    , draggedValueType = None
    , rangeStartValue = 0
    , thumbStartingPosition = 0
    , thumbParentWidth = 0
    , dragStartPosition = 0
    , formatter = config.formatter
    }


{-| takes a model and a message and applies it to create an updated model
-}
update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update message model =
    case message of
        RangeChanged valueType newValue shouldFetchModels ->
            let
                convertedValue =
                    String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0

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
                    snapValue (String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0) model.step

                newModel =
                    case valueType of
                        LowValue ->
                            { model | lowValue = correctMin convertedValue model.lowValue model.min }

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


{-| renders the current values using the formatter
-}
formatCurrentValue : Model -> String
formatCurrentValue model =
    if model.lowValue == model.min && model.highValue == model.max then
        ""
    else
        (model.formatter model.lowValue) ++ " - " ++ (model.formatter model.highValue)


snapValue : Float -> Int -> Float
snapValue value step =
    toFloat (((round value) // step) * step)


correctMin : Float -> Float -> Float -> Float
correctMin convertedValue lowValue minValue =
    if convertedValue > lowValue then
        convertedValue
    else
        convertedValue + minValue


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
                    toString (round (model.max / rectangle.width) * mouseX)
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.int)
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
                        centerThreshold =
                            rectangle.width / 2

                        refValue =
                            model.highValue - model.lowValue

                        newValue =
                            snapValue ((((model.highValue - model.lowValue) / rectangle.width) * mouseX) + model.lowValue) model.step
                    in
                        toString (round newValue)
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
        Mouse.position
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
            100 / model.max

        progressLow =
            toString (model.lowValue * progressRatio) ++ "%"

        progressHigh =
            toString ((model.max - model.highValue) * progressRatio) ++ "%"
    in
        div []
            [ div
                [ Html.Attributes.class "input-range-container" ]
                [ Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min (toString model.min)
                    , Html.Attributes.max (toString model.max)
                    , Html.Attributes.value <| (toString model.lowValue)
                    , Html.Attributes.step (toString model.step)
                    , Html.Attributes.class "input-range input-range--first"
                    , Html.Events.on "change" (onRangeChange LowValue True)
                    , Html.Events.on "input" (onRangeChange LowValue False)
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min (toString model.min)
                    , Html.Attributes.max (toString model.max)
                    , Html.Attributes.value <| (toString model.highValue)
                    , Html.Attributes.step (toString model.step)
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
                    , Html.Attributes.style [ ( "left", progressLow ), ( "right", progressHigh ) ]
                    , Html.Events.on "click" (onInsideRangeClick model)
                    ]
                    []
                ]
            , div
                [ Html.Attributes.class "input-range-labels-container" ]
                [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.formatter model.min) ]
                , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ] [ Html.text (formatCurrentValue model) ]
                , div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.formatter model.max) ]
                ]
            ]


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
            toString ((model.lowValue - model.min) * progressRatio) ++ "%"

        highThumbStartingPosition =
            toString ((model.highValue - model.min) * progressRatio) ++ "%"

        progressLow =
            toString ((model.lowValue - model.min) * progressRatio) ++ "%"

        progressHigh =
            toString ((model.max - model.highValue) * progressRatio) ++ "%"
    in
        div []
            [ div
                [ Html.Attributes.class "input-range-container" ]
                [ div
                    [ Html.Attributes.class "slider-thumb slider-thumb--first"
                    , Html.Attributes.style [ ( "left", lowThumbStartingPosition ), ( "float", "left" ) ]
                    , Html.Events.onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (onThumbMouseDown LowValue)
                    ]
                    []
                , div
                    [ Html.Attributes.class "slider-thumb slider-thumb--second"
                    , Html.Attributes.style [ ( "left", highThumbStartingPosition ), ( "float", "left" ) ]
                    , Html.Events.onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (onThumbMouseDown HighValue)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__track"
                    , Html.Events.on "click" (onOutsideRangeClick model)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__progress"
                    , Html.Attributes.style [ ( "left", progressLow ), ( "right", progressHigh ) ]
                    , Html.Events.on "click" (onInsideRangeClick model)
                    ]
                    []
                ]
            , div
                [ Html.Attributes.class "input-range-labels-container" ]
                [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.formatter model.min) ]
                , div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.formatter model.max) ]
                ]
            ]


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dragging then
        Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
    else
        Sub.none
