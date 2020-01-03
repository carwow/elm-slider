module RangeSlider exposing (CommonAttributes, DoubleSlider, SingleSlider, ValueAttributes, defaultCurrentRangeFormatter, defaultFormatter, defaultValueFormatter, doubleUpdate, doubleView, initDoubleSlider, initSingleSlider, singleUpdate, singleView)

import Browser.Events exposing (..)
import DOM exposing (boundingClientRect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- STATE


type SingleSlider msg
    = SingleSlider
        { commonAttributes : CommonAttributes msg
        , valueAttributes : ValueAttributes msg
        }


type DoubleSlider msg
    = DoubleSlider
        { commonAttributes : CommonAttributes msg
        , lowValueAttributes : ValueAttributes msg
        , highValueAttributes : ValueAttributes msg
        , currentRangeFormatter : { lowValue : Float, highValue : Float, min : Float, max : Float } -> String
        }


type alias ValueAttributes msg =
    { change : Float -> msg
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
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.min <| String.fromFloat commonAttributes.min
        , Html.Attributes.max <| String.fromFloat commonAttributes.max
        , Html.Attributes.step <| String.fromFloat commonAttributes.step
        , Html.Attributes.value <| String.fromFloat valueAttributes.value
        , Html.Attributes.class "input-range"
        , onChange valueAttributes.change
        ]
        []


singleView : SingleSlider msg -> Html msg
singleView (SingleSlider slider) =
    div []
        [ sliderInputView slider.commonAttributes slider.valueAttributes
        , sliderTrackView slider.commonAttributes
        , sliderProgressView slider.commonAttributes slider.valueAttributes
        , div
            [ Html.Attributes.class "input-range-labels-container" ]
            [ div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text <| slider.commonAttributes.minFormatter { value = slider.commonAttributes.min } ]
            , div
                [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text <| slider.valueAttributes.formatter { value = slider.valueAttributes.value, max = slider.commonAttributes.max } ]
            , div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text <| slider.commonAttributes.maxFormatter { value = slider.commonAttributes.max } ]
            ]
        ]


doubleView : DoubleSlider msg -> Html msg
doubleView (DoubleSlider slider) =
    div []
        [ div [ Html.Attributes.class "input-range-container" ]
            [ sliderInputView slider.commonAttributes slider.lowValueAttributes
            , sliderInputView slider.commonAttributes slider.highValueAttributes
            , sliderTrackView slider.commonAttributes
            , sliderProgressView slider.commonAttributes slider.lowValueAttributes
            ]
        , div [ Html.Attributes.class "input-range-labels-container" ]
            [ div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text (slider.commonAttributes.minFormatter { value = slider.commonAttributes.min }) ]
            , div
                [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text (formatCurrentRange (DoubleSlider slider)) ]
            , div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text (slider.commonAttributes.maxFormatter { value = slider.commonAttributes.max }) ]
            ]
        ]


formatCurrentRange : DoubleSlider msg -> String
formatCurrentRange (DoubleSlider slider) =
    slider.currentRangeFormatter
        { lowValue = slider.lowValueAttributes.value
        , highValue = slider.highValueAttributes.value
        , min = slider.commonAttributes.min
        , max = slider.commonAttributes.max
        }


sliderTrackView : CommonAttributes msg -> Html msg
sliderTrackView commonAttributes =
    div [ Html.Attributes.class "input-range__track", onClick (onOutsideRangeClick commonAttributes) ] []


sliderProgressView : CommonAttributes msg -> ValueAttributes msg -> Html msg
sliderProgressView commonAttributes valueAttributes =
    let
        progressPercentages =
            calculateProgressPercentages commonAttributes valueAttributes

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style "left" <| String.fromFloat progressPercentages.left ++ "%"
            , Html.Attributes.style "right" <| String.fromFloat progressPercentages.right ++ "%"
            , onClick (onInsideRangeClick commonAttributes valueAttributes)
            ]
    in
    div progressAttributes []



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


defaultCurrentRangeFormatter : { lowValue : Float, highValue : Float, min : Float, max : Float } -> String
defaultCurrentRangeFormatter values =
    if values.lowValue == values.min && values.highValue == values.max then
        ""

    else
        String.join " " [ String.fromFloat values.lowValue, "-", String.fromFloat values.highValue ]


initSingleSlider :
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , onChange : Float -> msg
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
            , click = attrs.onChange
            , minFormatter = attrs.minFormatter
            , maxFormatter = attrs.maxFormatter
            }
        , valueAttributes =
            { value = attrs.value
            , change = attrs.onChange
            , formatter = attrs.valueFormatter
            }
        }


initDoubleSlider :
    { min : Float
    , max : Float
    , step : Float
    , lowValue : Float
    , highValue : Float
    , onLowChange : Float -> msg
    , onHighChange : Float -> msg
    , valueFormatter : { value : Float, max : Float } -> String
    , minFormatter : { value : Float } -> String
    , maxFormatter : { value : Float } -> String
    , currentRangeFormatter : { lowValue : Float, highValue : Float, min : Float, max : Float } -> String
    }
    -> DoubleSlider msg
initDoubleSlider attrs =
    DoubleSlider
        { commonAttributes =
            { min = attrs.min
            , max = attrs.max
            , step = attrs.step
            , click = attrs.onLowChange
            , minFormatter = attrs.minFormatter
            , maxFormatter = attrs.maxFormatter
            }
        , lowValueAttributes =
            { value = attrs.lowValue
            , change = attrs.onLowChange
            , formatter = attrs.valueFormatter
            }
        , highValueAttributes =
            { value = attrs.highValue
            , change = attrs.onHighChange
            , formatter = attrs.valueFormatter
            }
        , currentRangeFormatter = attrs.currentRangeFormatter
        }


singleUpdate : Float -> SingleSlider msg -> SingleSlider msg
singleUpdate value (SingleSlider ({ valueAttributes } as slider)) =
    SingleSlider
        { valueAttributes = { valueAttributes | value = value }
        , commonAttributes = slider.commonAttributes
        }


doubleUpdate : { lowValue : Maybe Float, highValue : Maybe Float } -> DoubleSlider msg -> DoubleSlider msg
doubleUpdate values (DoubleSlider ({ lowValueAttributes, highValueAttributes } as slider)) =
    case ( values.lowValue, values.highValue ) of
        ( Nothing, Nothing ) ->
            DoubleSlider slider

        ( Just lowValue, Nothing ) ->
            DoubleSlider
                { commonAttributes = slider.commonAttributes
                , lowValueAttributes =
                    { lowValueAttributes
                        | value = Basics.min lowValue (slider.highValueAttributes.value - slider.commonAttributes.step)
                    }
                , highValueAttributes =
                    highValueAttributes
                , currentRangeFormatter = slider.currentRangeFormatter
                }

        ( Nothing, Just highValue ) ->
            DoubleSlider
                { commonAttributes = slider.commonAttributes
                , lowValueAttributes = lowValueAttributes
                , highValueAttributes =
                    { highValueAttributes
                        | value = Basics.max highValue (slider.lowValueAttributes.value - slider.commonAttributes.step)
                    }
                , currentRangeFormatter = slider.currentRangeFormatter
                }

        ( Just lowValue, Just highValue ) ->
            DoubleSlider
                { commonAttributes = slider.commonAttributes
                , lowValueAttributes =
                    { lowValueAttributes
                        | value = Basics.min lowValue (slider.highValueAttributes.value - slider.commonAttributes.step)
                    }
                , highValueAttributes =
                    { highValueAttributes
                        | value = Basics.max highValue (slider.lowValueAttributes.value - slider.commonAttributes.step)
                    }
                , currentRangeFormatter = slider.currentRangeFormatter
                }
