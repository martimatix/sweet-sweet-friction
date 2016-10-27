module Circle.Growth exposing (..)

import Circle exposing (Circle)
import Circle.Collision as CC


grow : List Circle -> Circle -> Circle
grow stationaryCircles movingCircle =
    case CC.collisionCircle movingCircle stationaryCircles of
        Nothing ->
            { movingCircle | radius = movingCircle.radius + 1 }

        _ ->
            movingCircle
