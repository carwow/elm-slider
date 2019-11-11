module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import RangeSlider exposing (..)


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { singleSlider : RangeSlider.Slider Msg
    , doubleSlider : RangeSlider.Slider Msg
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { singleSlider = RangeSlider.defaultSingleSlider handleSliderInput handleSliderChange
            , doubleSlider = RangeSlider.defaultDoubleSlider handleSliderInput handleSliderChange handleSliderInput handleSliderChange
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | SliderInput String
    | SliderChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SliderInput str ->
            let
                newSlider =
                    RangeSlider.update { value = String.toFloat str, lowValue = Nothing, highValue = Nothing } model.singleSlider
            in
            ( { model | singleSlider = newSlider }, Cmd.none )

        SliderChange str ->
            let
                newSlider =
                    RangeSlider.update { value = String.toFloat str, lowValue = Nothing, highValue = Nothing } model.singleSlider
            in
            ( { model | singleSlider = newSlider }, Cmd.none )



-- VIEW


handleSliderInput : String -> Msg
handleSliderInput str =
    SliderInput str


handleSliderChange : String -> Msg
handleSliderChange str =
    SliderChange str


view : Model -> Html Msg
view model =
    div []
        [ RangeSlider.view model.singleSlider
        , RangeSlider.view model.doubleSlider
        ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
