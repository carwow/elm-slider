module RangeSlider exposing (CommonAttributes, DoubleSlider, SingleSlider, ValueAttributes, defaultFormatter, defaultValueFormatter, doubleUpdate, doubleView, initSingleSlider, singleUpdate, singleView)

import Browser.Events exposing (..)
import DOM exposing (boundingClientRect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- STATE


type SingleSlider msg
    = SingleSlider { commonAttributes : CommonAttributes msg, valueAttributes : ValueAttributes msg }


type DoubleSlider msg
    = DoubleSlider { commonAttributes : CommonAttributes msg, lowValueAttributes : ValueAttributes msg, highValueAttributes : ValueAttributes msg }


type alias ValueAttributes msg =
    { change : Float -> msg
    , input : Float -> msg
    , value : Float
    , formatter : { value : Float, max : Float } -> String
    }


type alias CommonAttributes msg =
    { max : Float
    , min : Float
    , step : Float
    , click : Float -> msg
    , minFormatter : { value : Float } -> String
    , maxFormatter : { value : Float } -> String
    }



-- Internals


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


snapValue : Float -> CommonAttributes msg -> Float
snapValue value model =
    let
        roundedStep =
            round model.step

        adjustedRoundedStep =
            if roundedStep > 0 then
                roundedStep

            else
                1

        newValue =
            value / toFloat adjustedRoundedStep

        roundedValue =
            floor newValue

        nextValue =
            toFloat (roundedValue * adjustedRoundedStep)
    in
    nextValue


onOutsideRangeClick : CommonAttributes msg -> Json.Decode.Decoder msg
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
                    toFloat newValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map model.click valueDecoder


onInsideRangeClick : CommonAttributes msg -> ValueAttributes msg -> Json.Decode.Decoder msg
onInsideRangeClick model value =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        adjustedValue =
                            clamp model.min model.max value.value

                        newValue =
                            round <|
                                adjustedValue
                                    - ((mouseX / rectangle.width) * (adjustedValue - model.min))

                        adjustedNewValue =
                            clamp model.min model.max <| toFloat newValue
                    in
                    adjustedNewValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map model.click valueDecoder


onClick : Json.Decode.Decoder msg -> Html.Attribute msg
onClick decoder =
    Html.Events.on "click" decoder


onChange : (Float -> msg) -> Html.Attribute msg
onChange msg =
    Html.Events.on "change" (Json.Decode.map msg inputDecoder)


onInput : (Float -> msg) -> Html.Attribute msg
onInput msg =
    Html.Events.on "input" (Json.Decode.map msg inputDecoder)


inputDecoder : Json.Decode.Decoder Float
inputDecoder =
    Json.Decode.map (\value -> Maybe.withDefault 0 <| String.toFloat value)
        Html.Events.targetValue


calculateProgressPercentages : CommonAttributes msg -> ValueAttributes msg -> { left : Float, right : Float }
calculateProgressPercentages commonAttributes valueAttributes =
    let
        progressRatio =
            100 / (commonAttributes.max - commonAttributes.min)

        value =
            clamp commonAttributes.min commonAttributes.max valueAttributes.value
    in
    { left = 0.0, right = (commonAttributes.max - value) * progressRatio }


sliderInputView : CommonAttributes msg -> ValueAttributes msg -> Html msg
sliderInputView commonAttributes valueAttributes =
    let
        track =
            [ Html.Attributes.class "input-range__track", onClick (onOutsideRangeClick commonAttributes) ]

        progressPercentages =
            calculateProgressPercentages commonAttributes valueAttributes

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style "left" <| String.fromFloat progressPercentages.left ++ "%"
            , Html.Attributes.style "right" <| String.fromFloat progressPercentages.right ++ "%"
            , onClick (onInsideRangeClick commonAttributes valueAttributes)
            ]
    in
    div [ Html.Attributes.class "input-range-container" ]
        [ Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min <| String.fromFloat commonAttributes.min
            , Html.Attributes.max <| String.fromFloat commonAttributes.max
            , Html.Attributes.step <| String.fromFloat commonAttributes.step
            , Html.Attributes.value <| String.fromFloat valueAttributes.value
            , Html.Attributes.class "input-range"
            , onChange valueAttributes.change
            , onInput valueAttributes.input
            ]
            []
        , div track []
        , div progressAttributes []
        ]


sliderLabelView : CommonAttributes msg -> ValueAttributes msg -> Html msg
sliderLabelView commonAttributes valueAttributes =
    div
        [ Html.Attributes.class "input-range-labels-container" ]
        [ div
            [ Html.Attributes.class "input-range-label" ]
            [ Html.text <| commonAttributes.minFormatter { value = commonAttributes.min } ]
        , div
            [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
            [ Html.text <| valueAttributes.formatter { value = valueAttributes.value, max = commonAttributes.max } ]
        , div
            [ Html.Attributes.class "input-range-label" ]
            [ Html.text <| commonAttributes.maxFormatter { value = commonAttributes.max } ]
        ]



-- API


defaultFormatter : { value : Float } -> String
defaultFormatter value =
    String.fromFloat value.value


defaultValueFormatter : { value : Float, max : Float } -> String
defaultValueFormatter values =
    if values.value == values.max then
        ""

    else
        String.fromFloat values.value


initSingleSlider :
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , onChange : Float -> msg
    , onInput : Float -> msg
    , onClick : Float -> msg
    , valueFormatter : { value : Float, max : Float } -> String
    , minFormatter : { value : Float } -> String
    , maxFormatter : { value : Float } -> String
    }
    -> SingleSlider msg
initSingleSlider attrs =
    SingleSlider
        { commonAttributes =
            { min = attrs.min
            , max = attrs.max
            , step = attrs.step
            , click = attrs.onClick
            , minFormatter = attrs.minFormatter
            , maxFormatter = attrs.maxFormatter
            }
        , valueAttributes =
            { value = attrs.value
            , change = attrs.onChange
            , input = attrs.onInput
            , formatter = attrs.valueFormatter
            }
        }


initDoubleSlider :
    { min : Float
    , max : Float
    , step : Float
    , lowValue : Float
    , highValue : Float
    , onChange : Float -> msg
    , onInput : Float -> msg
    , onClick : Float -> msg
    , valueFormatter : { value : Float, max : Float } -> String
    , minFormatter : { value : Float } -> String
    , maxFormatter : { value : Float } -> String
    }
    -> DoubleSlider msg
initDoubleSlider attrs =
    DoubleSlider
        { commonAttributes =
            { min = attrs.min
            , max = attrs.max
            , step = attrs.step
            , click = attrs.onClick
            , minFormatter = attrs.minFormatter
            , maxFormatter = attrs.maxFormatter
            }
        , lowValueAttributes =
            { value = attrs.lowValue
            , change = attrs.onChange
            , input = attrs.onInput
            , formatter = attrs.valueFormatter
            }
        , highValueAttributes =
            { value = attrs.highValue
            , change = attrs.onChange
            , input = attrs.onInput
            , formatter = attrs.valueFormatter
            }
        }


singleUpdate : Float -> SingleSlider msg -> SingleSlider msg
singleUpdate value (SingleSlider ({ valueAttributes } as slider)) =
    SingleSlider
        { valueAttributes = { valueAttributes | value = value }
        , commonAttributes = slider.commonAttributes
        }


doubleUpdate : Float -> Float -> DoubleSlider msg -> DoubleSlider msg
doubleUpdate lowValue highValue (DoubleSlider ({ lowValueAttributes, highValueAttributes } as slider)) =
    DoubleSlider
        { commonAttributes = slider.commonAttributes
        , lowValueAttributes =
            { lowValueAttributes
                | value = lowValue
            }
        , highValueAttributes =
            { highValueAttributes
                | value = highValue
            }
        }


singleView : SingleSlider msg -> Html msg
singleView (SingleSlider slider) =
    div []
        [ sliderInputView slider.commonAttributes slider.valueAttributes
        , sliderLabelView slider.commonAttributes slider.valueAttributes
        ]


doubleView : DoubleSlider msg -> Html msg
doubleView (DoubleSlider slider) =
    div []
        [ sliderInputView slider.commonAttributes slider.lowValueAttributes
        , sliderInputView slider.commonAttributes slider.highValueAttributes
        , div
            []
            [ sliderLabelView slider.commonAttributes slider.lowValueAttributes
            , sliderLabelView slider.commonAttributes slider.highValueAttributes
            ]
        ]
