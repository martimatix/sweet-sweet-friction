module Model exposing (..)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds exposing (Bounds)


type alias Model =
    { stationaryCircles : List Circle
    , movingCircle : Circle
    , velocity : Vector
    , bounds : Bounds
    }


initial : Model
initial =
    Model [ (Circle 20 50 20), (Circle 250 250 30) ] (Circle 60 200 50) ( 0, -0.5 ) ( 500, 600 )
