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
    { singleSlider : RangeSlider.SingleSlider Msg
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { singleSlider =
                RangeSlider.initSingleSlider
                    { min = 0
                    , max = 1000
                    , value = 500
                    , step = 50
                    , onChange = handleSliderChange
                    , onInput = handleSliderInput
                    , onClick = handleSliderChange
                    , valueFormatter = RangeSlider.defaultValueFormatter
                    , minFormatter = RangeSlider.defaultFormatter
                    , maxFormatter = RangeSlider.defaultFormatter
                    }
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | SliderInput Float
    | SliderChange Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SliderInput str ->
            let
                newSlider =
                    RangeSlider.singleUpdate
                        str
                        model.singleSlider
            in
            ( { model | singleSlider = newSlider }, Cmd.none )

        SliderChange str ->
            let
                newSlider =
                    RangeSlider.singleUpdate
                        str
                        model.singleSlider
            in
            ( { model | singleSlider = newSlider }, Cmd.none )



-- VIEW


handleSliderInput : Float -> Msg
handleSliderInput str =
    SliderInput str


handleSliderChange : Float -> Msg
handleSliderChange str =
    SliderChange str


view : Model -> Html Msg
view model =
    div []
        [ RangeSlider.singleView model.singleSlider
        ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
