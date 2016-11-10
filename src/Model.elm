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
    Model []
        (initialCircle 0)
        ( 0, 0 )
        0
        Waiting


initialCircle : Int -> Circle
initialCircle rotation =
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
        (Circle cx cy 15 3 rotation)
