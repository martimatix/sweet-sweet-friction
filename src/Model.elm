module Model exposing (..)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds exposing (Bounds)


type alias Model =
    { stationaryCircles : List Circle
    , movingCircle : Circle
    , velocity : Vector
    , bounds : Bounds
    , ticks : Int
    , state : State
    }


type State
    = Waiting
    | Travelling
    | Growing


initial : Model
initial =
    Model [ (Circle 20 50 20 1), (Circle 250 250 30 2) ]
        initialCircle
        ( 0, 0 )
        ( 500, 600 )
        0
        Waiting


initialCircle : Circle
initialCircle =
    (Circle 175 500 15 3)
