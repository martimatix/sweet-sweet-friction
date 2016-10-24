module Model exposing (..)

import CircularCollision exposing (Circle)
import Vector exposing (Vector)


type alias Model =
    { stationaryCircle : Circle
    , movingCircle : Circle
    , velocity : Vector
    }


initial : Model
initial =
    Model (Circle -30 50 50) (Circle 50 500 50) ( 0, -1 )
