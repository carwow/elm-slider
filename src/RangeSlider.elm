module RangeSlider exposing (Config, view)

import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- STATE


{-| Tracks the current value of the slider
-}
type State
    = State Float


initialValue : Float -> State
initialValue value =
    State value


type Config a
    = Config
        { change : String -> a
        , input : String -> a
        , max : Float
        , min : Float
        , step : Float
        }


config :
    { change : State -> a
    , input : State -> a
    , max : Float
    , min : Float
    , step : Float
    , value : Float
    }
    -> Config a
config { change, input, max, min, step, value } =
    Config
        { change = change
        , input = input
        , max = max
        , min = min
        , step = step
        , value = value
        }


initRangeSlider : String -> (String -> a) -> (String -> a) -> RangeSlider a
initRangeSlider id change input =
    RangeSlider
        { id = id
        , config = defaultConfig change input
        }



-- Config


type alias Config a =
    { change : String -> a
    , input : String -> a
    , max : Float
    , min : Float
    , step : Float
    , value : Float
    }


defaultConfig : (String -> a) -> (String -> a) -> Config a
defaultConfig change input =
    { change = change
    , input = input
    , max = 1000
    , min = 0
    , step = 100
    , value = 500
    }



-- View


onChange : (String -> a) -> Html.Attribute a
onChange msg =
    Html.Events.on "change" (Json.Decode.map msg Html.Events.targetValue)


onInput : (String -> a) -> Html.Attribute a
onInput msg =
    Html.Events.on "input" (Json.Decode.map msg Html.Events.targetValue)


view : RangeSlider a -> Html a
view (RangeSlider slider) =
    div []
        [ div
            [ Html.Attributes.class "input-range-container" ]
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min <| String.fromFloat slider.config.min
                , Html.Attributes.max <| String.fromFloat slider.config.max
                , Html.Attributes.step <| String.fromFloat slider.config.step
                , Html.Attributes.class "input-range"
                , Html.Attributes.style "direction" <|
                    "ltr"
                , onChange slider.config.change
                , onInput slider.config.input
                ]
                []
            ]
        , div
            [ Html.Attributes.class "input-range-labels-container" ]
            [ div [ Html.Attributes.class "input-range-label" ] []
            , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text <| String.fromFloat slider.config.value ]
            , div [ Html.Attributes.class "input-range-label" ] []
            ]
        ]
