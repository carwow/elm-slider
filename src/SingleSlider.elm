module SingleSlider exposing
    ( Model, defaultModel, ProgressDirection(..)
    , Msg, update, subscriptions
    , view
    )

{-| A single slider built natively in Elm


# Model

@docs Model, defaultModel, ProgressDirection


# Update

@docs Msg, update, subscriptions


# View

@docs view

-}

import DOM exposing (boundingClientRect)
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode exposing (map)


{-| The base model for the slider
-}
type alias Model =
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , minFormatter : Float -> String
    , maxFormatter : Float -> String
    , currentValueFormatter : Float -> Float -> String
    , disabled : Bool
    , progressDirection : ProgressDirection
    , reversed : Bool
    }


{-| The basic type accepted by the update
-}
type Msg
    = TrackClicked String
    | RangeChanged String Bool


{-| Progress Bar direction (left or right)
-}
type ProgressDirection
    = ProgressLeft
    | ProgressRight


{-| Default model
-}
defaultModel : Model
defaultModel =
    { min = 0
    , max = 100
    , step = 10
    , value = 0
    , minFormatter = String.fromFloat
    , maxFormatter = String.fromFloat
    , currentValueFormatter = defaultCurrentValueFormatter
    , disabled = False
    , progressDirection = ProgressLeft
    , reversed = False
    }


{-| Default formatter for the current value
-}
defaultCurrentValueFormatter : Float -> Float -> String
defaultCurrentValueFormatter currentValue max =
    if currentValue == max then
        ""

    else
        String.fromFloat currentValue


{-| takes a model and a message and applies it to create an updated model
-}
update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update message model =
    case message of
        RangeChanged newValue shouldFetchModels ->
            let
                convertedValue =
                    String.toFloat newValue |> Maybe.withDefault 0

                newModel =
                    { model | value = convertedValue }
            in
            ( newModel, Cmd.none, shouldFetchModels )

        TrackClicked newValue ->
            let
                convertedValue =
                    snapValue (String.toFloat newValue |> Maybe.withDefault model.min) model

                newModel =
                    { model | value = convertedValue }
            in
            ( newModel, Cmd.none, True )


closestStep : Float -> Float -> Int
closestStep value step =
    let
        roundedValue =
            round value

        roundedStep =
            if round step > 0 then
                round step

            else
                1

        remainder =
            remainderBy roundedStep roundedValue
    in
    if remainder > (roundedStep // 2) then
        (roundedValue - remainder) + roundedStep

    else
        roundedValue - remainder


snapValue : Float -> Model -> Float
snapValue value model =
    let
        roundedStep =
            round model.step

        adjustedRoundedStep =
            if roundedStep > 0 then
                roundedStep

            else
                1

        newValue =
            value / toFloat adjustedRoundedStep

        roundedValue =
            case model.progressDirection of
                ProgressLeft ->
                    floor newValue

                ProgressRight ->
                    ceiling newValue

        nextValue =
            toFloat (roundedValue * adjustedRoundedStep)
    in
    nextValue


onOutsideRangeClick : Model -> Json.Decode.Decoder Msg
onOutsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        clickedValue =
                            if model.reversed then
                                model.max - (((model.max - model.min) / rectangle.width) * mouseX)
                            else
                                (((model.max - model.min) / rectangle.width) * mouseX) + model.min

                        newValue =
                            closestStep clickedValue model.step
                    in
                    String.fromInt newValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map TrackClicked valueDecoder


onInsideRangeClick : Model -> Json.Decode.Decoder Msg
onInsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        adjustedValue =
                            clamp model.min model.max model.value

                        newValue =
                            round <|
                                case model.progressDirection of
                                    ProgressLeft ->
                                        if model.reversed then
                                            model.max - (model.max - adjustedValue) * (mouseX / rectangle.width)
                                        else
                                            (adjustedValue - model.min) * (mouseX / rectangle.width) + model.min

                                    ProgressRight ->
                                        if model.reversed then
                                            adjustedValue - ((mouseX / rectangle.width) * (adjustedValue - model.min))
                                        else
                                            adjustedValue + ((mouseX / rectangle.width) * (model.max - adjustedValue))

                        adjustedNewValue =
                            clamp model.min model.max <| toFloat newValue
                    in
                    String.fromFloat adjustedNewValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map TrackClicked valueDecoder


onRangeChange : Bool -> Json.Decode.Decoder Msg
onRangeChange shouldFetchModels =
    Json.Decode.map2
        RangeChanged
        targetValue
        (Json.Decode.succeed shouldFetchModels)


{-| Displays the slider
-}
view : Model -> Html Msg
view model =
    let
        trackAttributes =
            [ Html.Attributes.class "input-range__track" ]

        trackAllAttributes =
            case model.disabled of
                False ->
                    List.append trackAttributes [ Html.Events.on "click" (onOutsideRangeClick model) ]

                True ->
                    trackAttributes

        progressPercentages =
            calculateProgressPercentages model

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style "left" <| String.fromFloat progressPercentages.left ++ "%"
            , Html.Attributes.style "right" <| String.fromFloat progressPercentages.right ++ "%"
            ]

        progressAllAttributes =
            case model.disabled of
                False ->
                    List.append progressAttributes [ Html.Events.on "click" (onInsideRangeClick model) ]

                True ->
                    progressAttributes

        (leftText, rightText) =
            if model.reversed then
                (model.maxFormatter model.max, model.minFormatter model.min)
            else
                (model.minFormatter model.min, model.maxFormatter model.max)

    in
    div []
        [ div
            [ Html.Attributes.class "input-range-container" ]
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min (String.fromFloat model.min)
                , Html.Attributes.max (String.fromFloat model.max)
                , Html.Attributes.value <| String.fromFloat model.value
                , Html.Attributes.step (String.fromFloat model.step)
                , Html.Attributes.class "input-range"
                , Html.Attributes.disabled model.disabled
                , Html.Events.on "change" (onRangeChange True)
                , Html.Events.on "input" (onRangeChange False)
                , Html.Attributes.style "direction" <| if model.reversed then "rtl" else "ltr"
                ]
                []
            , div
                trackAllAttributes
                []
            , div
                progressAllAttributes
                []
            ]
        , div
            [ Html.Attributes.class "input-range-labels-container" ]
            [ div [ Html.Attributes.class "input-range-label" ] [ Html.text leftText ]
            , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text (model.currentValueFormatter model.value model.max) ]
            , div [ Html.Attributes.class "input-range-label" ] [ Html.text rightText ]
            ]
        ]


{-| Returns the percentage adjusted min, max values for the range (actual min - actual max)
-}
calculateProgressPercentages : Model -> { left : Float, right : Float }
calculateProgressPercentages model =
    let
        progressRatio =
            100 / (model.max - model.min)

        value =
            clamp model.min model.max model.value
    in
    case (model.progressDirection, model.reversed) of
        (ProgressRight, False) ->
            { left = (value - model.min) * progressRatio, right = 0.0 }

        (ProgressLeft, False) ->
            { left = 0.0, right = (model.max - value) * progressRatio }

        (ProgressLeft, True) ->
            { left = 0.0, right = 100 - (model.max - value) * progressRatio }

        (ProgressRight, True) ->
            { left = 100 - (value - model.min) * progressRatio, right = 0.0 }



-- Subscriptions ---------------------------------------------------------------


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
