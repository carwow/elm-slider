module RangeSlider exposing (Config, SliderAttributes, SliderType, defaultSingleSlider, updateDoubleSliderHighValue, updateDoubleSliderLowValue, updateSingleSliderValue, view)

import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- STATE


type Config a
    = Config (SliderType a)


type SliderType a
    = Single (SliderAttributes a)
    | Double { low : SliderAttributes a, high : SliderAttributes a }


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


sliderView : SliderAttributes a -> Html a
sliderView attributes =
    div []
        [ div
            [ Html.Attributes.class "input-range-container" ]
            [ Html.input
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
            ]
        , div
            [ Html.Attributes.class "input-range-labels-container" ]
            [ div [ Html.Attributes.class "input-range-label" ] []
            , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text <| String.fromFloat attributes.value ]
            , div [ Html.Attributes.class "input-range-label" ] []
            ]
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


defaultSingleSlider : (String -> a) -> (String -> a) -> Config a
defaultSingleSlider change input =
    Config (Single <| defaultSliderAttributes change input)


defaultDoubleSlider : (String -> a) -> (String -> a) -> (String -> a) -> (String -> a) -> Config a
defaultDoubleSlider lowChange lowInput highChange highInput =
    Config (Double <| { low = defaultSliderAttributes lowChange lowInput, high = defaultSliderAttributes highChange highInput })


updateSingleSliderValue : Float -> Config a -> Config a
updateSingleSliderValue value (Config slider) =
    case slider of
        Single attributes ->
            let
                updatedAttributes =
                    { attributes | value = value }
            in
            Config (Single updatedAttributes)

        Double attributes ->
            Config slider


updateDoubleSliderLowValue : Float -> Config a -> Config a
updateDoubleSliderLowValue value (Config slider) =
    case slider of
        Single attributes ->
            Config slider

        Double attributes ->
            let
                lowAttributes =
                    attributes.low

                updatedAttributes =
                    { attributes | low = { lowAttributes | value = value } }
            in
            Config (Double updatedAttributes)


updateDoubleSliderHighValue : Float -> Config a -> Config a
updateDoubleSliderHighValue value (Config slider) =
    case slider of
        Single attributes ->
            Config slider

        Double attributes ->
            let
                highAttributes =
                    attributes.high

                updatedAttributes =
                    { attributes | high = { highAttributes | value = value } }
            in
            Config (Double updatedAttributes)


view : Config a -> Html a
view (Config slider) =
    case slider of
        Single attributes ->
            div [] [ sliderView attributes ]

        Double attributes ->
            div [] [ sliderView attributes.low, sliderView attributes.high ]
