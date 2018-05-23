-- Vendored from https://github.com/debois/elm-dom
-- Once that package supports 0.19, this should be removed, and the functions from that package used
-- instead


module DOM exposing (boundingClientRect)

import Json.Decode as Decode exposing (Decoder, field, andThen)


type alias Rectangle =
    { top : Float
    , left : Float
    , width : Float
    , height : Float
    }


boundingClientRect : Decoder Rectangle
boundingClientRect =
    Decode.map3
        (\( x, y ) width height ->
            { top = y
            , left = x
            , width = width
            , height = height
            }
        )
        (position 0 0)
        offsetWidth
        offsetHeight


position : Float -> Float -> Decoder ( Float, Float )
position x y =
    Decode.map4
        (\scrollLeft_ scrollTop_ offsetLeft_ offsetTop_ ->
            ( x + offsetLeft_ - scrollLeft_, y + offsetTop_ - scrollTop_ )
        )
        scrollLeft
        scrollTop
        offsetLeft
        offsetTop
        |> andThen
            (\( x_, y_ ) ->
                offsetParent ( x_, y_ ) (position x_ y_)
            )


offsetWidth : Decoder Float
offsetWidth =
    field "offsetWidth" Decode.float


offsetHeight : Decoder Float
offsetHeight =
    field "offsetHeight" Decode.float


scrollLeft : Decoder Float
scrollLeft =
    field "scrollLeft" Decode.float


scrollTop : Decoder Float
scrollTop =
    field "scrollTop" Decode.float


offsetParent : a -> Decoder a -> Decoder a
offsetParent x decoder =
    Decode.oneOf
        [ field "offsetParent" <| Decode.null x
        , field "offsetParent" decoder
        ]


offsetTop : Decoder Float
offsetTop =
    field "offsetTop" Decode.float


offsetLeft : Decoder Float
offsetLeft =
    field "offsetLeft" Decode.float
