module Model exposing (..)

import CircularCollision exposing (Circle)
import Vector exposing (Vector)


type alias Bounds =
    ( Int, Int )


type alias Model =
    { stationaryCircle : Circle
    , movingCircle : Circle
    , velocity : Vector
    , bounds : ( Int, Int )
    }


initial : Model
initial =
    Model (Circle 20 50 20) (Circle 60 200 50) ( 0, -2 ) ( 300, 500 )
