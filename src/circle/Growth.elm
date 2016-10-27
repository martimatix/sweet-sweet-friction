module Circle.Growth exposing (..)

import Bounds exposing (Bounds)
import Circle.Collision as CC
import WallCollision as WC


grow : List Circle -> Bounds -> Circle -> Circle
grow stationaryCircles bounds movingCircle =
    movingCircle
