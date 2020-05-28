module RangeSlider exposing (CommonAttributes, ValueAttributes, defaultLabelFormatter, defaultValueFormatter, onClick, sliderInputView, sliderTrackView)

import Html exposing (Html, div)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on)
import Json.Decode


type alias ValueAttributes msg =
    { change : Float -> msg
    , value : Float
    , formatter : Float -> Float -> String
    }


type alias CommonAttributes =
    { max : Float
    , min : Float
    , step : Float
    , minFormatter : Float -> String
    , maxFormatter : Float -> String
    }


onChange : (Float -> msg) -> Json.Decode.Decoder Float -> Html.Attribute msg
onChange msg input =
    Html.Events.on "change" (Json.Decode.map msg input)


onInput : (Float -> msg) -> Json.Decode.Decoder Float -> Html.Attribute msg
onInput msg input =
    Html.Events.on "input" (Json.Decode.map msg input)


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
        , onInput valueAttributes.change input
        ]
        []


sliderTrackView : Json.Decode.Decoder msg -> Html msg
sliderTrackView decoder =
    div [ Html.Attributes.class "input-range__track", onClick decoder ] []


onClick : Json.Decode.Decoder msg -> Html.Attribute msg
onClick decoder =
    Html.Events.on "click" decoder


defaultLabelFormatter : Float -> String
defaultLabelFormatter value =
    String.fromFloat value


defaultValueFormatter : Float -> Float -> String
defaultValueFormatter value max =
    if value == max then
        ""

    else
        String.fromFloat value
