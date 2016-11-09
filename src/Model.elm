module Model exposing (..)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds exposing (Bounds)


type alias Model =
    { stationaryCircles : List Circle
    , activeCircle : Circle
    , velocity : Vector
    , ticks : Int
    , state : State
    }


type State
    = Waiting
    | Travelling
    | Growing
    | GameOver


initial : Model
initial =
    Model [ (Circle 20 50 20 1), (Circle 250 250 30 2) ]
        initialCircle
        ( 0, 0 )
        0
        Waiting


initialCircle : Circle
initialCircle =
    let
        ( x, y ) =
            Bounds.game

        cx =
            (x // 2)
                |> toFloat

        cy =
            (y - distanceOfCannonFromFloor)
                |> toFloat

        distanceOfCannonFromFloor =
            20
    in
        (Circle cx cy 15 3)
