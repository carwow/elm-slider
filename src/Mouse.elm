module Mouse exposing (Position, positionDecoder)

import Json.Decode


type alias Position =
    { x : Int
    , y : Int
    }


positionDecoder : Json.Decode.Decoder Position
positionDecoder =
    Json.Decode.map2 Position
        (Json.Decode.field "pageX" Json.Decode.int)
        (Json.Decode.field "pageY" Json.Decode.int)
