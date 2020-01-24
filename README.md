# elm-slider

A range slider component, implemented natively in Elm.

## Installation

```shell
elm install carwow/elm-slider
```

## Usage

For a full example implementation, see `examples/Main.elm`

There are two types of sliders that can be rendered, a `SingleSlider`, with one track thumb and a `DoubleSlider`, with two track thumbs.

Input handling is *your* responsibility - define a function to decided what to do when the slider's value changes.

### SingleSlider Example

```elm
singleSlider =
      SingleSlider.init
          { min = 0
          , max = 1000
          , value = 500
          , step = 50
          , onChange = handleSingleSliderChange
          }


handleSingleSliderChange : Float -> Msg
handleSingleSliderChange str =
    SingleSliderChange str



view : Model -> Html Msg
view model =
    div []
        [ div [] [ SingleSlider.view model.singleSlider ] ]
```

### DoubleSliderExample

```elm
doubleSlider =
    DoubleSlider.init
        { min = 0
        , max = 1000
        , lowValue = 500
        , highValue = 750
        , step = 50
        , onLowChange = handleDoubleSliderLowChange
        , onHighChange = handleDoubleSliderHighChange
        }
        
        
handleDoubleSliderLowChange : Float -> Msg
handleDoubleSliderLowChange str =
    DoubleSliderLowChange str


handleDoubleSliderHighChange : Float -> Msg
handleDoubleSliderHighChange str =
    DoubleSliderHighChange str


view : Model -> Html Msg
view model =
    div []
        [ div [] [ DoubleSlider.view model.doubleSlider ] ]
```

### Using a custom formatter

You can use a custom min label formatter, max label formatter, or value formatter like below:

```elm
singleSlider =
      SingleSlider.init
          { min = 0
          , max = 1000
          , value = 500
          , step = 50
          , onChange = handleSingleSliderChange
          }
          |> SingleSlider.withMinFormatter minFormatter
```
