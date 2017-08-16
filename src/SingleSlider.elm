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
                    snapValue (String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0) model.step

                newModel =
                    { model | value = convertedValue }
            in
                ( newModel, Cmd.none, True )

        DragStart position offsetLeft ->
            ( { model
                | dragging = True
                , rangeStartValue = model.value
                , thumbStartingPosition = offsetLeft + 8
                , dragStartPosition = (toFloat position.x)
              }
            , Cmd.none
            , False
            )

        DragAt position ->
            let
                delta =
                    ((toFloat position.x) - model.dragStartPosition)

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

        DragEnd position ->
            let
                _ =
                    Debug.log "position" position

                _ =
                    Debug.log "model" model
            in
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
                    toString (round ((model.max / rectangle.width) * mouseX))
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
                    toString (round ((model.value / rectangle.width) * mouseX))
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map TrackClicked valueDecoder


onThumbMouseDown : Json.Decode.Decoder Msg
onThumbMouseDown =
    Json.Decode.map2
        DragStart
        Mouse.position
        (Json.Decode.at [ "target", "offsetLeft" ] Json.Decode.float)


{-| Displays the slider
-}
view : Model -> Html Msg
view model =
    let
        progress_ratio =
            100 / model.max

        thumbStartingPosition =
            toString (model.value * progress_ratio) ++ "%"

        progress =
            toString ((model.max - model.value) * progress_ratio) ++ "%"
    in
        div
            [ Html.Attributes.class "input-range-container" ]
            [ div
                [ Html.Attributes.class "slider-thumb slider-thumb--first"
                , Html.Attributes.style [ ( "left", thumbStartingPosition ) ]
                , Html.Events.onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } onThumbMouseDown
                ]
                []
            , div
                [ Html.Attributes.class "input-range__track"
                , Html.Attributes.style [ ( "z-index", "1" ) ]
                , Html.Events.on "click" (onOutsideRangeClick model)
                ]
                []
            , div
                [ Html.Attributes.class "input-range__progress"
                , Html.Attributes.style [ ( "left", "0" ), ( "right", progress ), ( "z-index", "1" ) ]
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
        Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
    else
        Sub.none
