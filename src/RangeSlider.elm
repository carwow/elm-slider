module RangeSlider exposing (Slider, SliderAttributes, defaultDoubleSlider, defaultSingleSlider, update, view)

import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- STATE


type Slider a
    = SingleSlider (SliderAttributes a)
    | DoubleSlider { low : SliderAttributes a, high : SliderAttributes a }


type alias SliderAttributes a =
    { change : String -> a
    , input : String -> a
    , max : Float
    , min : Float
    , step : Float
    , value : Float
    }



-- Internals


onChange : (String -> a) -> Html.Attribute a
onChange msg =
    Html.Events.on "change" (Json.Decode.map msg Html.Events.targetValue)


onInput : (String -> a) -> Html.Attribute a
onInput msg =
    Html.Events.on "input" (Json.Decode.map msg Html.Events.targetValue)


sliderInputView : SliderAttributes a -> Html a
sliderInputView attributes =
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.min <| String.fromFloat attributes.min
        , Html.Attributes.max <| String.fromFloat attributes.max
        , Html.Attributes.step <| String.fromFloat attributes.step
        , Html.Attributes.class "input-range"
        , Html.Attributes.style "direction" <| "ltr"
        , onChange attributes.change
        , onInput attributes.input
        ]
        []


sliderLabelView : SliderAttributes a -> Html a
sliderLabelView attributes =
    div
        [ Html.Attributes.class "input-range-labels-container" ]
        [ div [ Html.Attributes.class "input-range-label" ] []
        , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
            [ Html.text <| String.fromFloat attributes.value ]
        , div [ Html.Attributes.class "input-range-label" ] []
        ]



-- API


defaultSliderAttributes : (String -> a) -> (String -> a) -> SliderAttributes a
defaultSliderAttributes change input =
    { max = 1000
    , min = 0
    , step = 100
    , value = 500
    , change = change
    , input = input
    }


defaultSingleSlider : (String -> a) -> (String -> a) -> Slider a
defaultSingleSlider change input =
    SingleSlider <| defaultSliderAttributes change input


defaultDoubleSlider : (String -> a) -> (String -> a) -> (String -> a) -> (String -> a) -> Slider a
defaultDoubleSlider lowChange lowInput highChange highInput =
    DoubleSlider <| { low = defaultSliderAttributes lowChange lowInput, high = defaultSliderAttributes highChange highInput }


update : { value : Maybe Float, lowValue : Maybe Float, highValue : Maybe Float } -> Slider a -> Slider a
update attrs slider =
    case slider of
        SingleSlider attributes ->
            SingleSlider { attributes | value = Maybe.withDefault attributes.value attrs.value }

        DoubleSlider { low, high } ->
            DoubleSlider
                { low = { low | value = Maybe.withDefault low.value attrs.lowValue }
                , high = { high | value = Maybe.withDefault high.value attrs.highValue }
                }


view : Slider a -> Html a
view slider =
    case slider of
        SingleSlider attributes ->
            div [] [ sliderInputView attributes, sliderLabelView attributes ]

        DoubleSlider attributes ->
            div []
                [ sliderInputView attributes.low
                , sliderInputView attributes.high
                , div [] [ sliderLabelView attributes.low, sliderLabelView attributes.high ]
                ]
