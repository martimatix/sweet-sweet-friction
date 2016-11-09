module Model exposing (..)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds exposing (Bounds)


type alias Model =
    { stationaryCircles : List Circle
    , activeCircle : Circle
    , velocity : Vector
    , gameBounds : Bounds
    , safeBounds : Bounds
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
        gameBounds
        safeBounds
        0
        Waiting


initialCircle : Circle
initialCircle =
    let
        ( x, y ) =
            gameBounds

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


gameBounds : Bounds
gameBounds =
    ( 500, 600 )


safeBounds : Bounds
safeBounds =
    let
        ( x, y ) =
            gameBounds

        cannonMargin =
            150
    in
        ( x, y - cannonMargin )
