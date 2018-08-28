module Model exposing (Model, State(..), initial, initialCircle, initialRadius)

import Bounds
import Circle exposing (Circle)
import RadialBurst exposing (RadialBurst)
import Vector exposing (Vector)


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
    , backgroundTextOpacity : Float
    , debounceTicks : Int
    }


type State
    = Waiting
    | Travelling
    | Growing Float Int
    | GameOver


initial : Model
initial =
    let
        initialRotation =
            5

        windowMargin =
            16
    in
    { stationaryCircles = []
    , activeCircle = initialCircle initialRotation
    , radialBursts = []
    , velocity = ( 0, 0 )
    , ticks = 0
    , state = Waiting
    , score = 0
    , highScore = 0
    , windowWidth = windowMargin
    , windowHeight = windowMargin
    , backgroundTextOpacity = 1
    , debounceTicks = 0
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
    Circle cx cy initialRadius 3 rotation


initialRadius : Float
initialRadius =
    15
