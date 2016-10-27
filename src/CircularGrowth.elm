module CircularGrowth exposing (..)

import Bounds exposing (Bounds)
import CircularCollision as CC
import WallCollision as WC


grow : List Circle -> Bounds -> Circle -> Circle
grow stationaryCircles bounds movingCircle =
    movingCircle
