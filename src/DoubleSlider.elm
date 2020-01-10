module DoubleSlider exposing (DoubleSlider, defaultCurrentRangeFormatter, init, update, view)

import DOM exposing (boundingClientRect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode
import RangeSlider


type DoubleSlider msg
    = DoubleSlider
        { commonAttributes : RangeSlider.CommonAttributes
        , lowValueAttributes : RangeSlider.ValueAttributes msg
        , highValueAttributes : RangeSlider.ValueAttributes msg
        , currentRangeFormatter : { lowValue : Float, highValue : Float, min : Float, max : Float } -> String
        , overlapThreshold : Float
        }


type Thumb
    = High
    | Low


changeMsg : DoubleSlider msg -> Thumb -> (Float -> msg)
changeMsg (DoubleSlider slider) thumb =
    case thumb of
        Low ->
            slider.lowValueAttributes.change

        High ->
            slider.highValueAttributes.change


snapValue : Float -> Float -> Float
snapValue value step =
    (value / step) * step


onOutsideRangeClick : DoubleSlider msg -> Json.Decode.Decoder msg
onOutsideRangeClick (DoubleSlider ({ commonAttributes, lowValueAttributes, highValueAttributes } as slider)) =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            snapValue ((commonAttributes.max / rectangle.width) * mouseX) commonAttributes.step

                        valueType =
                            if newValue < lowValueAttributes.value then
                                Low

                            else
                                High
                    in
                    valueType
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            (((commonAttributes.max - commonAttributes.min) / rectangle.width) * mouseX) + commonAttributes.min
                    in
                    newValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map2 (changeMsg (DoubleSlider slider)) valueTypeDecoder valueDecoder


onInsideRangeClick : DoubleSlider msg -> Json.Decode.Decoder msg
onInsideRangeClick (DoubleSlider ({ commonAttributes, lowValueAttributes, highValueAttributes } as slider)) =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        centerThreshold =
                            rectangle.width / 2

                        valueType =
                            if mouseX < centerThreshold then
                                Low

                            else
                                High
                    in
                    valueType
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            snapValue ((((highValueAttributes.value - lowValueAttributes.value) / rectangle.width) * mouseX) + lowValueAttributes.value) commonAttributes.step
                    in
                    newValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map2 (changeMsg (DoubleSlider slider)) valueTypeDecoder valueDecoder


formatCurrentRange : DoubleSlider msg -> String
formatCurrentRange (DoubleSlider slider) =
    slider.currentRangeFormatter
        { lowValue = slider.lowValueAttributes.value
        , highValue = slider.highValueAttributes.value
        , min = slider.commonAttributes.min
        , max = slider.commonAttributes.max
        }


progressView : DoubleSlider msg -> Html msg
progressView (DoubleSlider ({ commonAttributes, lowValueAttributes, highValueAttributes } as slider)) =
    let
        lowValue =
            lowValueAttributes.value

        highValue =
            highValueAttributes.value

        progressRatio =
            100 / (commonAttributes.max - commonAttributes.min)

        progressLow =
            String.fromFloat ((lowValue - commonAttributes.min) * progressRatio) ++ "%"

        progressHigh =
            String.fromFloat ((commonAttributes.max - highValue) * progressRatio) ++ "%"
    in
    div
        [ Html.Attributes.class "input-range__progress"
        , Html.Attributes.style "left" progressLow
        , Html.Attributes.style "right" progressHigh
        , Html.Events.on "click" (onInsideRangeClick (DoubleSlider slider))
        ]
        []


inputDecoder : DoubleSlider msg -> Thumb -> Json.Decode.Decoder Float
inputDecoder (DoubleSlider slider) thumb =
    Json.Decode.map (\value -> String.toFloat value |> Maybe.withDefault 0 |> convertValue (DoubleSlider slider) thumb)
        Html.Events.targetValue


convertValue : DoubleSlider msg -> Thumb -> Float -> Float
convertValue (DoubleSlider slider) thumb value =
    case thumb of
        Low ->
            Basics.min value (slider.highValueAttributes.value - (slider.commonAttributes.step * slider.overlapThreshold))

        High ->
            Basics.max value (slider.lowValueAttributes.value + (slider.commonAttributes.step * slider.overlapThreshold))



-- API


defaultCurrentRangeFormatter : { lowValue : Float, highValue : Float, min : Float, max : Float } -> String
defaultCurrentRangeFormatter values =
    if values.lowValue == values.min && values.highValue == values.max then
        ""

    else
        String.join " " [ String.fromFloat values.lowValue, "-", String.fromFloat values.highValue ]


init :
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
    , overlapThreshold : Float
    }
    -> DoubleSlider msg
init attrs =
    DoubleSlider
        { commonAttributes =
            { min = attrs.min
            , max = attrs.max
            , step = attrs.step
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
        , overlapThreshold = attrs.overlapThreshold
        }


update : { lowValue : Maybe Float, highValue : Maybe Float } -> DoubleSlider msg -> DoubleSlider msg
update values (DoubleSlider ({ lowValueAttributes, highValueAttributes } as slider)) =
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
                , overlapThreshold = slider.overlapThreshold
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
                , overlapThreshold = slider.overlapThreshold
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
                , overlapThreshold = slider.overlapThreshold
                }


view : DoubleSlider msg -> Html msg
view (DoubleSlider slider) =
    div []
        [ div [ Html.Attributes.class "input-range-container" ]
            [ RangeSlider.sliderInputView slider.commonAttributes slider.lowValueAttributes (inputDecoder (DoubleSlider slider) Low)
            , RangeSlider.sliderInputView slider.commonAttributes slider.highValueAttributes (inputDecoder (DoubleSlider slider) High)
            , RangeSlider.sliderTrackView (onOutsideRangeClick (DoubleSlider slider))
            , progressView (DoubleSlider slider)
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
