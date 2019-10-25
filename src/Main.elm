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
    { slider : RangeSlider.Config Msg
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { slider = RangeSlider.defaultConfig handleSliderInput handleSliderChange }
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
                newModel =
                    { model | slider = RangeSlider.updateValue (String.toFloat str |> Maybe.withDefault 0) model.slider }
            in
            ( newModel, Cmd.none )

        SliderChange str ->
            let
                newModel =
                    { model | slider = RangeSlider.updateValue (String.toFloat str |> Maybe.withDefault 0) model.slider }
            in
            ( newModel, Cmd.none )



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
        [ RangeSlider.view model.slider ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
