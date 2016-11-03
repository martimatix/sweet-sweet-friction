module Circle exposing (Circle, advance)

import Vector exposing (Vector)


type alias Circle =
    { cx : Float
    , cy : Float
    , radius : Float
    , health : Int
    }


advance : Vector -> Circle -> Circle
advance ( x, y ) ({ cx, cy } as circle) =
    { circle | cx = (cx + x), cy = (cy + y) }
