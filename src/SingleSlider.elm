module SingleSlider exposing (Model, Msg, init, update, subscriptions, view)

{-| A single slider built natively in Elm


# Model

@docs Model


# Update

@docs Msg, update, subscriptions


# Configuring the slider

@docs init


# View

@docs view

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
    , value : Float
    , dragging : Bool
    , rangeStartValue : Float
    , thumbStartingPosition : Float
    , dragStartPosition : Float
    }


{-| The basic type accepted by the update
-}
type Msg
    = TrackClicked String
    | DragStart Position Float
    | DragAt Position
    | DragEnd Position


{-| Returns a default range slider
-}
init : { min : Float, max : Float, step : Int, value : Float } -> Model
init config =
    { min = config.min
    , max = config.max
    , step = config.step
    , value = config.value
    , dragging = False
    , rangeStartValue = 0
    , thumbStartingPosition = 0
    , dragStartPosition = 0
    }


{-| takes a model and a message and applies it to create an updated model
-}
update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update message model =
    case message of
        TrackClicked newValue ->
            let
                convertedValue =
                    snapValue (String.toFloat newValue |> Maybe.withDefault 0) model.step

                newModel =
                    { model | value = convertedValue }
            in
                ( newModel, Cmd.none, True )

        DragStart { x, y } offsetLeft ->
            ( { model
                | dragging = True
                , rangeStartValue = model.value
                , thumbStartingPosition = offsetLeft + 8
                , dragStartPosition = (toFloat x)
              }
            , Cmd.none
            , False
            )

        DragAt { x, y } ->
            let
                delta =
                    ((toFloat x) - model.dragStartPosition)

                ratio =
                    (model.rangeStartValue / model.thumbStartingPosition)

                newValue =
                    snapValue ((model.thumbStartingPosition + delta) * ratio) model.step

                newModel =
                    if newValue >= model.min && newValue <= model.max then
                        { model | value = newValue }
                    else
                        model
            in
                ( newModel, Cmd.none, False )

        DragEnd _ ->
            ( { model
                | dragging = False
                , rangeStartValue = 0
                , thumbStartingPosition = 0
                , dragStartPosition = 0
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
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    String.fromInt (round ((model.max / rectangle.width) * mouseX))
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
                    String.fromInt (round ((model.value / rectangle.width) * mouseX))
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map TrackClicked valueDecoder


{-| Displays the slider
-}
view : Model -> Html Msg
view model =
    let
        progress_ratio =
            100 / model.max

        thumbStartingPosition =
            String.fromFloat (model.value * progress_ratio) ++ "%"

        progress =
            String.fromFloat ((model.max - model.value) * progress_ratio) ++ "%"

        onThumbMouseDown : Json.Decode.Decoder Msg
        onThumbMouseDown =
            Json.Decode.map2
                DragStart
                positionDecoder
                (Json.Decode.at [ "target", "offsetLeft" ] Json.Decode.float)

        mouseDownEvent =
            onThumbMouseDown
                |> Json.Decode.map (\msg -> { message = msg, stopPropagation = True, preventDefault = True })
                |> Html.Events.custom "mousedown"
    in
        div
            [ Html.Attributes.class "input-range-container" ]
            [ div
                [ Html.Attributes.class "slider-thumb slider-thumb--first"
                , Html.Attributes.style "left" thumbStartingPosition
                , mouseDownEvent
                ]
                []
            , div
                [ Html.Attributes.class "input-range__track"
                , Html.Attributes.style "z-index" "1"
                , Html.Events.on "click" (onOutsideRangeClick model)
                ]
                []
            , div
                [ Html.Attributes.class "input-range__progress"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "right" progress
                , Html.Attributes.style "z-index" "1"
                , Html.Events.on "click" (onInsideRangeClick model)
                ]
                []
            ]



-- Subscriptions ---------------------------------------------------------------


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
