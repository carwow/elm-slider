# elm-slider

```shell
elm package install carwow/elm-slider
```

## Usage

The `DoubleSlider.init` function creates a double range slider which handles values from `min` to `max` with a `step`,
providing two thumbs with with values `lowValue` and `highValue`.

```elm
    slider =
        DoubleSlider.init
            { min = 50
            , max = 5000
            , step = 50
            , lowValue = 50
            , highValue = 5000
            , thumbOverlapThreshold = 1
            }
```

The `SingleSlider.init` function creates a range slider which handles values from `min` to `max` with a `step`.

```elm
    slider =
        SingleSlider.init
            { min = 50
            , max = 5000
            , step = 50
            , value = 2000
            }
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

On mobile we suggest to use the `fallbackView` as `view` doesn't handle touchEvents
```elm
DoubleSlider.fallbackView model.slider |> Html.map SliderMsg
```

## Example

[...]


## Css

[...]