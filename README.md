# elm-slider

```shell
elm install carwow/elm-slider
```

## Usage

You can create a double slider model which handles values from `min` to `max` with a `step`, providing two thumbs with with values `lowValue` and `highValue`.

```elm
    let
        initialSliderModel =
            DoubleSlider.defaultModel
    in
        { initialSliderModel
            | min = 50
            , max = 5000
            , step = 50
            , lowValue = 50
            , highValue = 5000
        }
```

Default formatters for the `min`, `max` and `current range` will be applied unless custom formatters are provided as the following:

```elm
    let
        initialSliderModel =
            DoubleSlider.defaultModel
    in
        { initialSliderModel
            | min = 50
            , max = 5000
            , step = 50
            , lowValue = 50
            , highValue = 5000
            , minFormatter = toString
            , maxFormatter = toString
            , currentRangeFormatter = customRangeFormatter
        }
```

where:

```elm
    customRangeFormatter : Float -> Float -> Float -> Float -> String
    customRangeFormatter lowValue highValue min max =
        ...
```

You can create a single slider model which handles values from `min` to `max` with a `step` and a `value`.

```elm
    let
        initialSliderModel =
            SingleSlider.defaultModel
    in
        { initialSliderModel
            | min = 50
            , max = 5000
            , step = 50
            , value = 2000
        }
```

Default formatters for the `min`, `max` and `current value` will be applied unless custom formatters are provided as the following:

```elm
    let
        initialSliderModel =
            SingleSlider.defaultModel
    in
        { initialSliderModel
            | min = 50
            , max = 5000
            , step = 50
            , lowValue = 50
            , highValue = 5000
            , minFormatter = toString
            , maxFormatter = toString
            , currentValueFormatter = customValueFormatter
        }
```

where:

```elm
    customValueFormatter : Float -> Float -> String
    customValueFormatter currentValue max =
        ...
```

Because it uses mouse movements, the range slider requires subscriptions. After initialization, handle the subscriptions.
```elm

subscriptions =
    Sub.map SliderMsg <|
            DoubleSlider.subscriptions model.slider
```

Handle the updates from the subscription in your main update function. Together with the new model and a command
the sliders update function returns also a boolean, which is false for all dragging updates and true when the
dragging stops. This is useful if you want to trigger expensive commands like api calls only after the dragging
has stopped.

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SliderMsg innerMsg ->
            let
                ( newSlider, cmd, updateResults ) =
                    DoubleSlider.update innerMsg model slider

                newModel =
                    { model | slider = newSlider }

                newCmd =
                    if updateResults then
                        Cmd.batch [ Cmd.map SliderMsg cmd, otherCmd ]
                    else
                        otherCmd
            in
                ( newModel, newCmd )
```

To view the slider, simply call the view function
```elm
DoubleSlider.view model.slider |> Html.map SliderMsg
```

## Example
```elm
module Thing exposing (init, update, subscriptions, view, Model, Msg)

import Html exposing (Html, div, text)
import SingleSlider as Slider exposing (..)

type alias Model =
  { slider : Slider.Model
  }

slider : Slider.Model
slider =
  let
    initialSlider =
      Slider.defaultModel
  in
    { initialSlider
        | min = 0
        , max = 10
        , step = 1
        , value = 0
    }

initialModel : Model
initialModel =
  { slider = slider
  }

type Msg
  = SliderMsg Slider.Msg


-- INIT
init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none)


-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SliderMsg sliderMsg ->
      let
        ( newSlider, cmd, updateResults ) =
          Slider.update sliderMsg model.slider

        newModel =
          { model | slider = newSlider }

        newCmd =
          if updateResults then
            Cmd.batch [ Cmd.map SliderMsg cmd, Cmd.none ]
          else
            Cmd.none
      in
        ( newModel, newCmd )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map SliderMsg <|
      Slider.subscriptions model.slider
    ]


-- VIEW
view : Model -> Html Msg
view model =
  div
    []
    [ Slider.view model.slider |> Html.map SliderMsg ]

```


## Css

[...]
