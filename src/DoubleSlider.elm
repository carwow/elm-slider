module DoubleSlider exposing
    ( DoubleSlider
    , init
    , view
    , updateHighValue, updateLowValue
    , withCurrentRangeFormatter, withHighValueFormatter, withLowValueFormatter, withMaxFormatter, withMinFormatter, withOverlapThreshold
    )

{-| A slider component, with two track thumbs.


# Definition

@docs DoubleSlider


# Init

@docs init


# View

@docs view


# Updaters

@docs updateHighValue, updateLowValue


# Config

@docs withCurrentRangeFormatter, withHighValueFormatter, withLowValueFormatter, withMaxFormatter, withMinFormatter, withOverlapThreshold

-}

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


defaultCurrentRangeFormatter : { lowValue : Float, highValue : Float, min : Float, max : Float } -> String
defaultCurrentRangeFormatter values =
    if values.lowValue == values.min && values.highValue == values.max then
        ""

    else
        String.join " " [ String.fromFloat values.lowValue, "-", String.fromFloat values.highValue ]



-- API


init :
    { min : Float
    , max : Float
    , step : Float
    , lowValue : Float
    , highValue : Float
    , onLowChange : Float -> msg
    , onHighChange : Float -> msg
    }
    -> DoubleSlider msg
init attrs =
    DoubleSlider
        { commonAttributes =
            { min = attrs.min
            , max = attrs.max
            , step = attrs.step
            , minFormatter = RangeSlider.defaultLabelFormatter
            , maxFormatter = RangeSlider.defaultLabelFormatter
            }
        , lowValueAttributes =
            { value = attrs.lowValue
            , change = attrs.onLowChange
            , formatter = RangeSlider.defaultValueFormatter
            }
        , highValueAttributes =
            { value = attrs.highValue
            , change = attrs.onHighChange
            , formatter = RangeSlider.defaultValueFormatter
            }
        , currentRangeFormatter = defaultCurrentRangeFormatter
        , overlapThreshold = 1.0
        }


withMinFormatter : (Float -> String) -> DoubleSlider msg -> DoubleSlider msg
withMinFormatter formatter (DoubleSlider ({ commonAttributes } as slider)) =
    DoubleSlider
        { lowValueAttributes = slider.lowValueAttributes
        , highValueAttributes = slider.highValueAttributes
        , currentRangeFormatter = slider.currentRangeFormatter
        , overlapThreshold = slider.overlapThreshold
        , commonAttributes = { commonAttributes | minFormatter = formatter }
        }


withMaxFormatter : (Float -> String) -> DoubleSlider msg -> DoubleSlider msg
withMaxFormatter formatter (DoubleSlider ({ commonAttributes } as slider)) =
    DoubleSlider
        { lowValueAttributes = slider.lowValueAttributes
        , highValueAttributes = slider.highValueAttributes
        , currentRangeFormatter = slider.currentRangeFormatter
        , overlapThreshold = slider.overlapThreshold
        , commonAttributes = { commonAttributes | maxFormatter = formatter }
        }


withLowValueFormatter : (Float -> Float -> String) -> DoubleSlider msg -> DoubleSlider msg
withLowValueFormatter formatter (DoubleSlider ({ lowValueAttributes } as slider)) =
    DoubleSlider
        { lowValueAttributes = { lowValueAttributes | formatter = formatter }
        , commonAttributes = slider.commonAttributes
        , highValueAttributes = slider.highValueAttributes
        , currentRangeFormatter = slider.currentRangeFormatter
        , overlapThreshold = slider.overlapThreshold
        }


withHighValueFormatter : (Float -> Float -> String) -> DoubleSlider msg -> DoubleSlider msg
withHighValueFormatter formatter (DoubleSlider ({ highValueAttributes } as slider)) =
    DoubleSlider
        { highValueAttributes = { highValueAttributes | formatter = formatter }
        , commonAttributes = slider.commonAttributes
        , lowValueAttributes = slider.lowValueAttributes
        , currentRangeFormatter = slider.currentRangeFormatter
        , overlapThreshold = slider.overlapThreshold
        }


withOverlapThreshold : Float -> DoubleSlider msg -> DoubleSlider msg
withOverlapThreshold overlapThreshold (DoubleSlider slider) =
    DoubleSlider
        { highValueAttributes = slider.highValueAttributes
        , commonAttributes = slider.commonAttributes
        , lowValueAttributes = slider.lowValueAttributes
        , currentRangeFormatter = slider.currentRangeFormatter
        , overlapThreshold = overlapThreshold
        }


withCurrentRangeFormatter : ({ lowValue : Float, highValue : Float, min : Float, max : Float } -> String) -> DoubleSlider msg -> DoubleSlider msg
withCurrentRangeFormatter currentRangeFormatter (DoubleSlider slider) =
    DoubleSlider
        { highValueAttributes = slider.highValueAttributes
        , commonAttributes = slider.commonAttributes
        , lowValueAttributes = slider.lowValueAttributes
        , currentRangeFormatter = currentRangeFormatter
        , overlapThreshold = slider.overlapThreshold
        }


updateLowValue : Float -> DoubleSlider msg -> DoubleSlider msg
updateLowValue value (DoubleSlider ({ lowValueAttributes, highValueAttributes, commonAttributes } as slider)) =
    DoubleSlider
        { commonAttributes = slider.commonAttributes
        , lowValueAttributes =
            { lowValueAttributes
                | value = Basics.min value (slider.highValueAttributes.value - commonAttributes.step)
            }
        , highValueAttributes = highValueAttributes
        , currentRangeFormatter = slider.currentRangeFormatter
        , overlapThreshold = slider.overlapThreshold
        }


updateHighValue : Float -> DoubleSlider msg -> DoubleSlider msg
updateHighValue value (DoubleSlider ({ lowValueAttributes, highValueAttributes, commonAttributes } as slider)) =
    DoubleSlider
        { commonAttributes = commonAttributes
        , lowValueAttributes = lowValueAttributes
        , highValueAttributes =
            { highValueAttributes
                | value = Basics.max value (lowValueAttributes.value - commonAttributes.step)
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
                [ Html.text (slider.commonAttributes.minFormatter slider.commonAttributes.min) ]
            , div
                [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text (formatCurrentRange (DoubleSlider slider)) ]
            , div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text (slider.commonAttributes.maxFormatter slider.commonAttributes.max) ]
            ]
        ]
