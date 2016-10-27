module Circle exposing (Circle, advance)

import Vector exposing (Vector)


type alias Circle =
    { cx : Float
    , cy : Float
    , radius : Float
    }


advance : Vector -> Circle -> Circle
advance ( x, y ) { cx, cy, radius } =
    Circle (cx + x) (cy + y) radius
