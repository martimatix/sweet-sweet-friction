module CircularCollision
    exposing
        ( Circle
        , collision
        )


type alias Circle =
    { cx : Float
    , cy : Float
    , radius : Float
    }


collision : Circle -> Circle -> Bool
collision circle1 circle2 =
    let
        dx =
            circle1.cx - circle2.cx

        dy =
            circle1.cy - circle2.cy

        distance =
            sqrt ((dx * dx) + (dy * dy))
    in
        if distance < circle1.radius + circle2.radius then
            True
        else
            False
