module Model exposing (..)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds
import RadialBurst exposing (RadialBurst)


type alias Model =
    { stationaryCircles : List Circle
    , activeCircle : Circle
    , radialBursts : List RadialBurst
    , velocity : Vector
    , ticks : Int
    , state : State
    , score : Int
    , highScore : Int
    , windowWidth : Int
    , windowHeight : Int
    }


type State
    = Waiting
    | Travelling
    | Growing Float Int
    | GameOver


initial : Model
initial =
    { stationaryCircles = []
    , activeCircle = (initialCircle 5)
    , radialBursts = []
    , velocity = ( 0, 0 )
    , ticks = 0
    , state = Waiting
    , score = 0
    , highScore = 0
    , windowWidth = 0
    , windowHeight = 0
    }


initialCircle : Int -> Circle
initialCircle rotation =
    let
        cx =
            (Bounds.gameX // 2)
                |> toFloat

        cy =
            (Bounds.gameY - distanceOfCannonFromFloor)
                |> toFloat

        distanceOfCannonFromFloor =
            20
    in
        (Circle cx cy initialRadius 3 rotation)


initialRadius : Float
initialRadius =
    15
