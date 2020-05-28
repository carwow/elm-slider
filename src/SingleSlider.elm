module SingleSlider exposing
    ( SingleSlider
    , init
    , view
    , update
    , withMaxFormatter, withMinFormatter, withValueFormatter
    , fetchValue
    )

{-| A slider component, with one track thumb.


# Definition

@docs SingleSlider


# Init

@docs init


# View

@docs view


# Update

@docs update


# Config

@docs withMaxFormatter, withMinFormatter, withValueFormatter


# Helper

@docs fetchValue

-}

import DOM exposing (boundingClientRect)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode
import RangeSlider


{-| Type representing the SingleSlider component
-}
type SingleSlider msg
    = SingleSlider
        { commonAttributes : RangeSlider.CommonAttributes
        , valueAttributes : RangeSlider.ValueAttributes msg
        }


onOutsideRangeClick : SingleSlider msg -> Json.Decode.Decoder msg
onOutsideRangeClick (SingleSlider ({ commonAttributes } as slider)) =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        clickedValue =
                            (((commonAttributes.max - commonAttributes.min) / rectangle.width) * mouseX) + commonAttributes.min
                    in
                    RangeSlider.snapValue clickedValue commonAttributes.step
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map slider.valueAttributes.change valueDecoder


onInsideRangeClick : SingleSlider msg -> Json.Decode.Decoder msg
onInsideRangeClick (SingleSlider { commonAttributes, valueAttributes }) =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        adjustedValue =
                            clamp commonAttributes.min commonAttributes.max valueAttributes.value

                        newValue =
                            round <| (adjustedValue / rectangle.width) * mouseX

                        adjustedNewValue =
                            clamp commonAttributes.min commonAttributes.max <| toFloat newValue
                    in
                    RangeSlider.snapValue adjustedNewValue commonAttributes.step
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
            (commonAttributes.max - value) * progressRatio

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style "left" <| "0.0%"
            , Html.Attributes.style "right" <| String.fromFloat progress ++ "%"
            , RangeSlider.onClick (onInsideRangeClick (SingleSlider slider))
            ]
    in
    div progressAttributes []


inputDecoder : Json.Decode.Decoder Float
inputDecoder =
    Json.Decode.map (\value -> String.toFloat value |> Maybe.withDefault 0)
        Html.Events.targetValue



-- API


{-| Initializes a SingleSlider
-}
init :
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , onChange : Float -> msg
    }
    -> SingleSlider msg
init attrs =
    SingleSlider
        { commonAttributes =
            { min = attrs.min
            , max = attrs.max
            , step = attrs.step
            , minFormatter = RangeSlider.defaultLabelFormatter
            , maxFormatter = RangeSlider.defaultLabelFormatter
            }
        , valueAttributes =
            { value = attrs.value
            , change = attrs.onChange
            , formatter = RangeSlider.defaultValueFormatter
            }
        }


{-| Allows for customization of the minimum value label
-}
withMinFormatter : (Float -> String) -> SingleSlider msg -> SingleSlider msg
withMinFormatter formatter (SingleSlider ({ commonAttributes } as slider)) =
    SingleSlider
        { valueAttributes = slider.valueAttributes
        , commonAttributes = { commonAttributes | minFormatter = formatter }
        }


{-| Allows for customization of the maximum value label
-}
withMaxFormatter : (Float -> String) -> SingleSlider msg -> SingleSlider msg
withMaxFormatter formatter (SingleSlider ({ commonAttributes } as slider)) =
    SingleSlider
        { valueAttributes = slider.valueAttributes
        , commonAttributes = { commonAttributes | maxFormatter = formatter }
        }


{-| Allows for customization of the current value label
-}
withValueFormatter : (Float -> Float -> String) -> SingleSlider msg -> SingleSlider msg
withValueFormatter formatter (SingleSlider ({ valueAttributes } as slider)) =
    SingleSlider
        { valueAttributes = { valueAttributes | formatter = formatter }
        , commonAttributes = slider.commonAttributes
        }


{-| Update the slider's value
-}
update : Float -> SingleSlider msg -> SingleSlider msg
update value (SingleSlider ({ valueAttributes } as slider)) =
    SingleSlider
        { valueAttributes = { valueAttributes | value = value }
        , commonAttributes = slider.commonAttributes
        }


{-| SingleSlider view
-}
view : SingleSlider msg -> Html msg
view (SingleSlider slider) =
    div []
        [ div [ class "input-range-container" ]
            [ RangeSlider.sliderInputView slider.commonAttributes slider.valueAttributes inputDecoder []
            , RangeSlider.sliderTrackView (onOutsideRangeClick (SingleSlider slider))
            , progressView (SingleSlider slider)
            ]
        , div
            [ Html.Attributes.class "input-range-labels-container" ]
            [ div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text <| slider.commonAttributes.minFormatter slider.commonAttributes.min ]
            , div
                [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text <| slider.valueAttributes.formatter slider.valueAttributes.value slider.commonAttributes.max ]
            , div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text <| slider.commonAttributes.maxFormatter slider.commonAttributes.max ]
            ]
        ]


{-| Fetch SingleSlider's value
-}
fetchValue : SingleSlider msg -> Float
fetchValue (SingleSlider { valueAttributes }) =
    valueAttributes.value
