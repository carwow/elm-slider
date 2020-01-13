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


## CSS

The is the base CSS for both single and double sliders. It is compatible with all major browsers including Internet Explorer 11.

We recommend to start with the following styles and override them according to the theme of your website.

Both sliders have a width set to 100% of the parent element. Therefore, in order to set a fixed width, we recommend to set it on the parent element and not override the width of the range slider. This is to ensure the flexibility of the component.

```
.input-range-container {
  display: inline-flex;
  align-items: center;
  position: relative;
  height: 48px;
}

.input-range-container,
.input-range {
  width: 100%;
}

.input-range,
.input-range:hover,
.input-range:focus {
  box-shadow: none;
}

.input-range {
  -webkit-appearance: none;
  background-color: transparent;
  padding: 0;
  overflow: visible;
  pointer-events: none;
  height: 48px;
  border: 0;
}

.input-range::-moz-focus-outer {
  border: 0;
}

.input-range::-webkit-slider-thumb {
  -webkit-appearance: none;
  height: 32px;
  width: 32px;
  border: none;
  background-color: white;
  border-radius: 100%;
  box-shadow: 0 0 0 2px rgba(33, 34, 36, 0.07);
  cursor: pointer;
  pointer-events: all;
  z-index: 2;
  position: relative;
}

.input-range::-moz-range-track {
  background: transparent;
}

.input-range::-moz-range-thumb {
  height: 32px;
  width: 32px;
  border: none;
  background-color: white;
  border-radius: 100%;
  box-shadow: 0 0 0 2px rgba(33, 34, 36, 0.07);
  cursor: pointer;
  pointer-events: all;
  z-index: 2;
  position: relative;
  transform: scale(1);
}

.input-range::-ms-track {
  background-color: transparent;
  border-color: transparent;
  color: transparent;
}

.input-range::-ms-fill-lower {
  background-color: transparent;
}

.input-range::-ms-thumb {
  height: 32px;
  width: 32px;
  border: none;
  background-color: white;
  border-radius: 100%;
  box-shadow: 0 0 0 2px rgba(33, 34, 36, 0.07);
  cursor: pointer;
  pointer-events: all;
  z-index: 2;
  position: relative;
}

.input-range:disabled, .input-range:disabled:hover {
  cursor: not-allowed;
  box-shadow: none;
  border: 0;
  background-color: transparent;
}

.input-range:disabled::-webkit-slider-thumb, .input-range:disabled:hover::-webkit-slider-thumb {
  cursor: not-allowed;
}

.input-range:disabled::-moz-range-thumb, .input-range:disabled:hover::-moz-range-thumb {
  cursor: not-allowed;
}

.input-range:disabled::-ms-thumb, .input-range:disabled:hover::-ms-thumb {
  cursor: not-allowed;
}

.input-range:disabled ~ .input-range__track, .input-range:disabled:hover ~ .input-range__track {
  cursor: not-allowed;
  background-color: #fafafa;
}

.input-range:disabled ~ .input-range__progress, .input-range:disabled:hover ~ .input-range__progress {
  cursor: not-allowed;
  background-color: #dcdee1;
}

.slider-thumb {
  height: 32px;
  width: 32px;
  border: none;
  background-color: white;
  border-radius: 100%;
  box-shadow: 0 0 0 2px rgba(33, 34, 36, 0.07);
  cursor: pointer;
  pointer-events: all;
  z-index: 2;
  position: relative;
  z-index: 2;
}

.slider-thumb--first {
  margin-left: -16px;
}

.slider-thumb--second {
  margin-left: -32px;
}

.input-range--first {
  position: absolute;
}

.input-range--second {
  position: relative;
}

.input-range__track,
.input-range__progress {
  border-radius: 8px;
  position: absolute;
  height: 8px;
  margin-top: -4px;
  top: 50%;
  z-index: 0;
}

.input-range__track:hover,
.input-range__progress:hover {
  cursor: pointer;
}

.input-range__track {
  background-color: #dcdee1;
  left: 0;
  right: 0;
}

.input-range__track:hover {
  cursor: pointer;
}

.input-range__progress {
  background-color: #00a4ff;
}

.input-range-labels-container {
  display: flex;
  justify-content: space-between;
}

.input-range-label {
  font-weight: bold;
}

.input-range-label--current-value {
  color: #00a4ff;
  text-align: center;
  flex: 2;
}

.input-range-label:first-child {
  text-align: left;
}

.input-range-label:last-child {
  text-align: right;
}
```
