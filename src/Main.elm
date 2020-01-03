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
    , doubleSlider : RangeSlider.DoubleSlider Msg
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
                    , valueFormatter = RangeSlider.defaultValueFormatter
                    , minFormatter = RangeSlider.defaultFormatter
                    , maxFormatter = RangeSlider.defaultFormatter
                    }
            , doubleSlider =
                RangeSlider.initDoubleSlider
                    { min = 0
                    , max = 1000
                    , lowValue = 500
                    , highValue = 750
                    , step = 50
                    , onLowChange = handleDoubleSliderLowChange
                    , onHighChange = handleDoubleSliderHighChange
                    , valueFormatter = RangeSlider.defaultValueFormatter
                    , minFormatter = RangeSlider.defaultFormatter
                    , maxFormatter = RangeSlider.defaultFormatter
                    , currentRangeFormatter = RangeSlider.defaultCurrentRangeFormatter
                    }
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | DoubleSliderLowChange Float
    | DoubleSliderHighChange Float
    | SliderChange Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DoubleSliderLowChange str ->
            let
                newSlider =
                    RangeSlider.doubleUpdate
                        { lowValue = Just str, highValue = Nothing }
                        model.doubleSlider
            in
            ( { model | doubleSlider = newSlider }, Cmd.none )

        DoubleSliderHighChange str ->
            let
                newSlider =
                    RangeSlider.doubleUpdate
                        { lowValue = Nothing, highValue = Just str }
                        model.doubleSlider
            in
            ( { model | doubleSlider = newSlider }, Cmd.none )

        SliderChange str ->
            let
                newSlider =
                    RangeSlider.singleUpdate
                        str
                        model.singleSlider
            in
            ( { model | singleSlider = newSlider }, Cmd.none )



-- VIEW


handleSliderChange : Float -> Msg
handleSliderChange str =
    SliderChange str


handleDoubleSliderLowChange : Float -> Msg
handleDoubleSliderLowChange str =
    DoubleSliderLowChange str


handleDoubleSliderHighChange : Float -> Msg
handleDoubleSliderHighChange str =
    DoubleSliderHighChange str


view : Model -> Html Msg
view model =
    div []
        [ RangeSlider.doubleView model.doubleSlider
        ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
