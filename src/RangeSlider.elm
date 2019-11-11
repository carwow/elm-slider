module RangeSlider exposing (Slider, SliderAttributes, defaultDoubleSlider, defaultSingleSlider, update, updateValue, view)

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


calculateProgressPercentages : SliderAttributes a -> { left : Float, right : Float }
calculateProgressPercentages model =
    let
        progressRatio =
            100 / (model.max - model.min)

        value =
            clamp model.min model.max model.value
    in
    { left = 0.0, right = (model.max - value) * progressRatio }


sliderInputView : SliderAttributes a -> Html a
sliderInputView attributes =
    let
        progressPercentages =
            calculateProgressPercentages attributes

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style "left" <| String.fromFloat progressPercentages.left ++ "%"
            , Html.Attributes.style "right" <| String.fromFloat progressPercentages.right ++ "%"
            ]
    in
    div [ Html.Attributes.class "input-range-container" ]
        [ Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min <| String.fromFloat attributes.min
            , Html.Attributes.max <| String.fromFloat attributes.max
            , Html.Attributes.step <| String.fromFloat attributes.step
            , Html.Attributes.class "input-range"
            , onChange attributes.change
            , onInput attributes.input
            ]
            []
        , div [ Html.Attributes.class "input-range__track" ] []
        , div progressAttributes []
        ]


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


update :
    { min : Maybe Float
    , max : Maybe Float
    , step : Maybe Float
    , value : Maybe Float
    , lowValue : Maybe Float
    , highValue : Maybe Float
    }
    -> Slider a
    -> Slider a
update attrs slider =
    case slider of
        SingleSlider attributes ->
            SingleSlider
                { attributes
                    | value = Maybe.withDefault attributes.value attrs.value
                    , min = Maybe.withDefault attributes.min attrs.min
                    , max = Maybe.withDefault attributes.max attrs.max
                    , step = Maybe.withDefault attributes.step attrs.step
                }

        DoubleSlider { low, high } ->
            DoubleSlider
                { low =
                    { low
                        | value = Maybe.withDefault low.value attrs.lowValue
                        , min = Maybe.withDefault low.min attrs.min
                        , max = Maybe.withDefault low.max attrs.max
                        , step = Maybe.withDefault low.step attrs.step
                    }
                , high =
                    { high
                        | value = Maybe.withDefault high.value attrs.highValue
                        , min = Maybe.withDefault high.min attrs.min
                        , max = Maybe.withDefault high.max attrs.max
                        , step = Maybe.withDefault high.step attrs.step
                    }
                }


updateValue : Maybe Float -> Slider a -> Slider a
updateValue value slider =
    case slider of
        SingleSlider attributes ->
            SingleSlider { attributes | value = Maybe.withDefault attributes.value value }

        DoubleSlider { low, high } ->
            DoubleSlider { low = low, high = high }


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
