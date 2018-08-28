module Circle exposing (Circle, advance, distanceBetweenCentres, vectorBetweenCentres)

import Vector exposing (Vector)


type alias Circle =
    { cx : Float
    , cy : Float
    , radius : Float
    , hitPoints : Int
    , rotation : Int
    }


advance : Vector -> Circle -> Circle
advance ( x, y ) ({ cx, cy } as circle) =
    { circle | cx = cx + x, cy = cy + y }


vectorBetweenCentres : Circle -> Circle -> Vector
vectorBetweenCentres circle1 circle2 =
    ( circle2.cx - circle1.cx, circle2.cy - circle1.cy )


distanceBetweenCentres : Circle -> Circle -> Float
distanceBetweenCentres circle1 circle2 =
    vectorBetweenCentres circle1 circle2 |> Vector.magnitude
