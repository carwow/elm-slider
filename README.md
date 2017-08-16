# elm-slider

## DoubleSlider Usage

```
model = { ..., doubleSlider : DoubleSlider.Model, ... }

Msg = ... | DoubleSliderEvent DoubleSlider.Msg | ...

init =
    ...
    doubleSliderModel = DoubleSlider.init { min = 0, max = 80000, step = 1000, value = 20000 }

model =
    { ...
    , doubleSlider = doubleSliderModel
    ...
    }

view =
    ...
    DoubleSlider.view model doubleSlider |> Html.map DoubleSliderEvent
    ...
update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of

        DoubleSliderEvent inner ->
            let
                ( newSlider, cmd, shouldFetchModels ) =
                    DoubleSlider.update inner model doubleSlider

                newModel =
                    { model | doubleSlider = newSlider }

                newCmd =
                    if shouldFetchModels then
                        Cmd.batch [ Cmd.map DoubleSliderEvent cmd, fetchResults newModel ]
                    else
                        Cmd.map DoubleSliderEvent cmd

            in
                ( newModel, newCmd )
```

## SingleSlider Usage

```
model = { ..., singleSlider : SingleSlider.Model, ... }

Msg = ... | SingleSliderEvent SingleSlider.Msg | ...

init =
    ...
    singleSliderModel = SingleSlider.init { min = 0, max = 80000, step = 1000, value = 20000 }

model =
    { ...
    , singleSlider = singleSliderModel
    ...
    }

view =
    ...
    SingleSlider.view model.singleSlider |> Html.map SingleSliderEvent
    ...
update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of

        SingleSliderEvent inner ->
            let
                ( newSlider, cmd, shouldFetchModels ) =
                    SingleSlider.update inner model.singleSlider

                newModel =
                    { model | singleSlider = newSlider }

                newCmd =
                    if shouldFetchModels then
                        Cmd.batch [ Cmd.map SingleSliderEvent cmd, fetchResults newModel ]
                    else
                        Cmd.map SingleSliderEvent cmd

            in
                ( newModel, newCmd )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map (SingleSliderEvent) <|
        SingleSlider.subscriptions model.singleSlider
```