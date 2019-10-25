module RangeSlider exposing (Config, defaultConfig, updateValue, view)

import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- STATE


type Config a
    = Config
        { change : String -> a
        , input : String -> a
        , max : Float
        , min : Float
        , step : Float
        , value : Float
        }


initConfig :
    { change : String -> a
    , input : String -> a
    , max : Float
    , min : Float
    , step : Float
    , value : Float
    }
    -> Config a
initConfig { change, input, max, min, step, value } =
    Config
        { change = change
        , input = input
        , max = max
        , min = min
        , step = step
        , value = value
        }


defaultConfig : (String -> a) -> (String -> a) -> Config a
defaultConfig change input =
    Config
        { change = change
        , input = input
        , max = 1000
        , min = 0
        , step = 100
        , value = 500
        }


updateValue : Float -> Config a -> Config a
updateValue newValue (Config config) =
    Config { config | value = newValue }



-- VIEW


onChange : (String -> a) -> Html.Attribute a
onChange msg =
    Html.Events.on "change" (Json.Decode.map msg Html.Events.targetValue)


onInput : (String -> a) -> Html.Attribute a
onInput msg =
    Html.Events.on "input" (Json.Decode.map msg Html.Events.targetValue)


view : Config a -> Html a
view (Config slider) =
    div []
        [ div
            [ Html.Attributes.class "input-range-container" ]
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min <| String.fromFloat slider.min
                , Html.Attributes.max <| String.fromFloat slider.max
                , Html.Attributes.step <| String.fromFloat slider.step
                , Html.Attributes.class "input-range"
                , Html.Attributes.style "direction" <| "ltr"
                , onChange slider.change
                , onInput slider.input
                ]
                []
            ]
        , div
            [ Html.Attributes.class "input-range-labels-container" ]
            [ div [ Html.Attributes.class "input-range-label" ] []
            , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text <| String.fromFloat slider.value ]
            , div [ Html.Attributes.class "input-range-label" ] []
            ]
        ]
