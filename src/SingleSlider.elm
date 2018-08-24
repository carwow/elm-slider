module SingleSlider exposing
    ( Model, defaultModel
    , Msg, update, subscriptions
    , view
    )

{-| A single slider built natively in Elm


# Model

@docs Model, defaultModel


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
    }


{-| The basic type accepted by the update
-}
type Msg
    = TrackClicked String
    | RangeChanged String Bool


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
                    String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0

                newModel =
                    { model | value = convertedValue }
            in
            ( newModel, Cmd.none, shouldFetchModels )

        TrackClicked newValue ->
            let
                convertedValue =
                    snapValue (String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0) model.step

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


snapValue : Float -> Float -> Float
snapValue value step =
    let
        roundedStep =
            if round step > 0 then
                round step

            else
                1
    in
    toFloat ((round value // roundedStep) * roundedStep)


onOutsideRangeClick : Model -> Json.Decode.Decoder Msg
onOutsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        clickedValue =
                            (((model.max - model.min) / rectangle.width) * mouseX) + model.min

                        newValue =
                            closestStep clickedValue model.step
                    in
                    String.fromFloat newValue
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
                    String.fromFloat (round ((model.value / rectangle.width) * mouseX))
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
        progress_ratio =
            100 / (model.max - model.min)

        progress =
            String.fromFloat ((model.max - model.value) * progress_ratio) ++ "%"

        trackAttributes =
            [ Html.Attributes.class "input-range__track" ]

        trackAllAttributes =
            case model.disabled of
                False ->
                    List.append trackAttributes [ Html.Events.on "click" (onOutsideRangeClick model) ]

                True ->
                    trackAttributes

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style [ ( "left", "0" ), ( "right", progress ) ]
            ]

        progressAllAttributes =
            case model.disabled of
                False ->
                    List.append progressAttributes [ Html.Events.on "click" (onInsideRangeClick model) ]

                True ->
                    progressAttributes
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
            [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.minFormatter model.min) ]
            , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text (model.currentValueFormatter model.value model.max) ]
            , div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.maxFormatter model.max) ]
            ]
        ]



-- Subscriptions ---------------------------------------------------------------


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
