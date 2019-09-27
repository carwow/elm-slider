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
    {}


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model, Cmd.none )



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
                debug =
                    Debug.log "Slider Input" str
            in
            ( model, Cmd.none )

        SliderChange str ->
            let
                debug =
                    Debug.log "Slider Change" str
            in
            ( model, Cmd.none )



-- VIEW


fetch : Model -> Cmd Msg
fetch model =
    Cmd.none


handleSliderInput : String -> Msg
handleSliderInput str =
    SliderInput str


handleSliderChange : String -> Msg
handleSliderChange str =
    SliderChange str


rangeSliderView : Html Msg
rangeSliderView =
    RangeSlider.initRangeSlider "id" handleSliderInput handleSliderChange |> RangeSlider.view


view : Model -> Html Msg
view model =
    div []
        [ rangeSliderView ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
