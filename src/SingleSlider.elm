module SingleSlider exposing (SingleSlider, init, update, view)

import DOM exposing (boundingClientRect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import RangeSlider


type SingleSlider msg
    = SingleSlider
        { commonAttributes : RangeSlider.CommonAttributes
        , valueAttributes : RangeSlider.ValueAttributes msg
        }


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


snapValue : Float -> SingleSlider msg -> Float
snapValue value (SingleSlider slider) =
    let
        roundedStep =
            round slider.commonAttributes.step

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


onOutsideRangeClick : SingleSlider msg -> Json.Decode.Decoder msg
onOutsideRangeClick (SingleSlider ({ commonAttributes, valueAttributes } as slider)) =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        clickedValue =
                            (((commonAttributes.max - commonAttributes.min) / rectangle.width) * mouseX) + commonAttributes.min

                        newValue =
                            closestStep clickedValue commonAttributes.step
                    in
                    toFloat newValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map slider.valueAttributes.change valueDecoder


onInsideRangeClick : SingleSlider msg -> Json.Decode.Decoder msg
onInsideRangeClick (SingleSlider ({ commonAttributes, valueAttributes } as slider)) =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        adjustedValue =
                            clamp commonAttributes.min commonAttributes.max valueAttributes.value

                        newValue =
                            round <|
                                adjustedValue
                                    - ((mouseX / rectangle.width) * (adjustedValue - commonAttributes.min))

                        adjustedNewValue =
                            clamp commonAttributes.min commonAttributes.max <| toFloat newValue
                    in
                    adjustedNewValue
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map valueAttributes.change valueDecoder


progressView : SingleSlider msg -> Html msg
progressView (SingleSlider ({ commonAttributes, valueAttributes } as slider)) =
    let
        progressRatio =
            100 / (commonAttributes.max - commonAttributes.min)

        value =
            clamp commonAttributes.min commonAttributes.max valueAttributes.value

        progress =
            commonAttributes.max - value * progressRatio

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style "left" <| String.fromFloat 0.0 ++ "%"
            , Html.Attributes.style "right" <| String.fromFloat progressRatio ++ "%"
            , RangeSlider.onClick (onInsideRangeClick (SingleSlider slider))
            ]
    in
    div progressAttributes []


inputDecoder : Json.Decode.Decoder Float
inputDecoder =
    Json.Decode.map (\value -> String.toFloat value |> Maybe.withDefault 0)
        Html.Events.targetValue



-- API


init :
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
init attrs =
    SingleSlider
        { commonAttributes =
            { min = attrs.min
            , max = attrs.max
            , step = attrs.step
            , minFormatter = attrs.minFormatter
            , maxFormatter = attrs.maxFormatter
            }
        , valueAttributes =
            { value = attrs.value
            , change = attrs.onChange
            , formatter = attrs.valueFormatter
            }
        }


update : Float -> SingleSlider msg -> SingleSlider msg
update value (SingleSlider ({ valueAttributes } as slider)) =
    SingleSlider
        { valueAttributes = { valueAttributes | value = value }
        , commonAttributes = slider.commonAttributes
        }


view : SingleSlider msg -> Html msg
view (SingleSlider slider) =
    div []
        [ RangeSlider.sliderInputView slider.commonAttributes slider.valueAttributes inputDecoder
        , RangeSlider.sliderTrackView (onOutsideRangeClick (SingleSlider slider))
        , progressView (SingleSlider slider)
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
