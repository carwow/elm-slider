module RangeSlider exposing (CommonAttributes, Slider, ValueAttributes, defaultDoubleSlider, defaultFormatter, defaultSingleSlider, defaultValueFormatter, update, updateValue, view)

import Browser.Events exposing (..)
import DOM exposing (boundingClientRect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- STATE


type Slider a
    = SingleSlider { commonAttributes : CommonAttributes a, valueAttributes : ValueAttributes a }
    | DoubleSlider { commonAttributes : CommonAttributes a, lowValueAttributes : ValueAttributes a, highValueAttributes : ValueAttributes a }


type alias ValueAttributes a =
    { change : String -> a
    , input : String -> a
    , value : Float
    , formatter : { value : Float, max : Float } -> String
    }


type alias CommonAttributes a =
    { max : Float
    , min : Float
    , step : Float
    , click : String -> a
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


snapValue : Float -> CommonAttributes a -> Float
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


onOutsideRangeClick : CommonAttributes a -> Json.Decode.Decoder a
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
                    String.fromInt newValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map model.click valueDecoder


onInsideRangeClick : CommonAttributes a -> ValueAttributes a -> Json.Decode.Decoder a
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
                    String.fromFloat adjustedNewValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map model.click valueDecoder


onChange : (String -> a) -> Html.Attribute a
onChange msg =
    Html.Events.on "change" (Json.Decode.map msg Html.Events.targetValue)


onInput : (String -> a) -> Html.Attribute a
onInput msg =
    Html.Events.on "input" (Json.Decode.map msg Html.Events.targetValue)


onClick : Json.Decode.Decoder a -> Html.Attribute a
onClick decoder =
    Html.Events.on "click" decoder


calculateProgressPercentages : CommonAttributes a -> ValueAttributes a -> { left : Float, right : Float }
calculateProgressPercentages commonAttributes valueAttributes =
    let
        progressRatio =
            100 / (commonAttributes.max - commonAttributes.min)

        value =
            clamp commonAttributes.min commonAttributes.max valueAttributes.value
    in
    { left = 0.0, right = (commonAttributes.max - value) * progressRatio }


sliderInputView : CommonAttributes a -> ValueAttributes a -> Html a
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


sliderLabelView : CommonAttributes a -> ValueAttributes a -> Html a
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


defaultCommonAttributes : (String -> a) -> CommonAttributes a
defaultCommonAttributes click =
    { max = 1000
    , min = 0
    , step = 100
    , click = click
    , minFormatter = defaultFormatter
    , maxFormatter = defaultFormatter
    }


defaultValueAttributes : (String -> a) -> (String -> a) -> ValueAttributes a
defaultValueAttributes change input =
    { value = 500
    , change = change
    , input = input
    , formatter = defaultValueFormatter
    }


defaultSingleSlider : (String -> a) -> (String -> a) -> (String -> a) -> Slider a
defaultSingleSlider click change input =
    SingleSlider
        { commonAttributes = defaultCommonAttributes click, valueAttributes = defaultValueAttributes change input }


defaultDoubleSlider : (String -> a) -> (String -> a) -> (String -> a) -> (String -> a) -> (String -> a) -> Slider a
defaultDoubleSlider click lowChange lowInput highChange highInput =
    DoubleSlider
        { commonAttributes = defaultCommonAttributes click
        , lowValueAttributes = defaultValueAttributes lowChange lowInput
        , highValueAttributes = defaultValueAttributes highChange highInput
        }


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
        SingleSlider { commonAttributes, valueAttributes } ->
            SingleSlider
                { commonAttributes =
                    { commonAttributes
                        | min = Maybe.withDefault commonAttributes.min attrs.min
                        , max = Maybe.withDefault commonAttributes.max attrs.max
                        , step = Maybe.withDefault commonAttributes.step attrs.step
                    }
                , valueAttributes =
                    { valueAttributes
                        | value = Maybe.withDefault valueAttributes.value attrs.value
                    }
                }

        DoubleSlider { commonAttributes, lowValueAttributes, highValueAttributes } ->
            DoubleSlider
                { commonAttributes =
                    { commonAttributes
                        | min = Maybe.withDefault commonAttributes.min attrs.min
                        , max = Maybe.withDefault commonAttributes.max attrs.max
                        , step = Maybe.withDefault commonAttributes.step attrs.step
                    }
                , lowValueAttributes =
                    { lowValueAttributes
                        | value = Maybe.withDefault lowValueAttributes.value attrs.lowValue
                    }
                , highValueAttributes =
                    { highValueAttributes
                        | value = Maybe.withDefault highValueAttributes.value attrs.highValue
                    }
                }


updateValue :
    { value : Maybe Float
    , lowValue : Maybe Float
    , highValue : Maybe Float
    }
    -> Slider a
    -> Slider a
updateValue values slider =
    case slider of
        SingleSlider { commonAttributes, valueAttributes } ->
            SingleSlider
                { valueAttributes = { valueAttributes | value = Maybe.withDefault valueAttributes.value values.value }
                , commonAttributes = commonAttributes
                }

        DoubleSlider { lowValueAttributes, highValueAttributes, commonAttributes } ->
            DoubleSlider
                { commonAttributes = commonAttributes
                , lowValueAttributes =
                    { lowValueAttributes
                        | value = Maybe.withDefault lowValueAttributes.value values.lowValue
                    }
                , highValueAttributes =
                    { highValueAttributes
                        | value = Maybe.withDefault highValueAttributes.value values.highValue
                    }
                }


view : Slider a -> Html a
view slider =
    case slider of
        SingleSlider { commonAttributes, valueAttributes } ->
            div []
                [ sliderInputView commonAttributes valueAttributes
                , sliderLabelView commonAttributes valueAttributes
                ]

        DoubleSlider { lowValueAttributes, highValueAttributes, commonAttributes } ->
            div []
                [ sliderInputView commonAttributes lowValueAttributes
                , sliderInputView commonAttributes highValueAttributes
                , div
                    []
                    [ sliderLabelView commonAttributes lowValueAttributes
                    , sliderLabelView commonAttributes highValueAttributes
                    ]
                ]
