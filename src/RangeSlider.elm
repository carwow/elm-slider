module RangeSlider exposing (CommonAttributes, ValueAttributes, defaultFormatter, defaultValueFormatter, onClick, sliderInputView, sliderTrackView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)


type alias ValueAttributes msg =
    { change : Float -> msg
    , value : Float
    , formatter : { value : Float, max : Float } -> String
    }


type alias CommonAttributes =
    { max : Float
    , min : Float
    , step : Float
    , minFormatter : { value : Float } -> String
    , maxFormatter : { value : Float } -> String
    }


onChange : (Float -> msg) -> Json.Decode.Decoder Float -> Html.Attribute msg
onChange msg input =
    Html.Events.on "change" (Json.Decode.map msg input)


sliderInputView : CommonAttributes -> ValueAttributes msg -> Json.Decode.Decoder Float -> Html msg
sliderInputView commonAttributes valueAttributes input =
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.min <| String.fromFloat commonAttributes.min
        , Html.Attributes.max <| String.fromFloat commonAttributes.max
        , Html.Attributes.step <| String.fromFloat commonAttributes.step
        , Html.Attributes.value <| String.fromFloat valueAttributes.value
        , Html.Attributes.class "input-range"
        , onChange valueAttributes.change input
        ]
        []


sliderTrackView : Json.Decode.Decoder msg -> Html msg
sliderTrackView decoder =
    div [ Html.Attributes.class "input-range__track", onClick decoder ] []


onClick : Json.Decode.Decoder msg -> Html.Attribute msg
onClick decoder =
    Html.Events.on "click" decoder


defaultFormatter : { value : Float } -> String
defaultFormatter value =
    String.fromFloat value.value


defaultValueFormatter : { value : Float, max : Float } -> String
defaultValueFormatter values =
    if values.value == values.max then
        ""

    else
        String.fromFloat values.value
