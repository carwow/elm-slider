# elm-slider

A range slider component, implemented natively in Elm.

## Installation

```shell
elm install carwow/elm-slider
```

## Live demo

You can check out the single and double sliders on this [demo on Ellie](https://ellie-app.com/bQyC4hkW3dra1).

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
        , onChange = SingleSliderChange
        }
          
          
type Msg
    = NoOp
    | SingleSliderChange Float


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
        , onLowChange = DoubleSliderLowChange
        , onHighChange = DoubleSliderHighChange
        }


type Msg
    = NoOp
    | DoubleSliderLowChange Float
    | DoubleSliderHighChange Float
        

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

## CSS

Example CSS for the slider components is provided at `examples/example.css`

It is recommended to start with these styles and override them according to the theme of your website.
