module Update exposing (update, Msg(..))

import Model exposing (..)
import Circle exposing (Circle)
import Circle.Collision as CC
import Circle.Growth as Growth exposing (State(..))
import WallCollision as WC
import Vector exposing (Vector)
import Friction exposing (Result(..))
import CannonAngle


type Msg
    = Tick Float
    | FireCannon


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            model
                |> animate
                |> wrapReturnType

        FireCannon ->
            case model.state of
                Waiting ->
                    { model
                        | state = Travelling
                        , velocity = initialVelocity model.ticks
                    }
                        ! []

                _ ->
                    model ! []


animate : Model -> Model
animate model =
    case model.state of
        Waiting ->
            model
                |> incrementTick

        Travelling ->
            model
                |> circularCollision
                |> wallCollision
                |> advanceCircle
                |> applyFriction

        Growing ->
            model
                |> growCircle


incrementTick : Model -> Model
incrementTick model =
    { model | ticks = model.ticks + 1 }


initialVelocity : Int -> Vector
initialVelocity ticks =
    let
        angle =
            ticks
                |> CannonAngle.ticksToAngle
                |> toFloat

        velocityMagnitude =
            3
    in
        ( 3 * cos (angle |> degrees), -3 * sin (angle |> degrees) )


circularCollision : Model -> Model
circularCollision ({ velocity, movingCircle, stationaryCircles } as model) =
    let
        nextVelocity =
            CC.nextVelocity velocity movingCircle stationaryCircles
    in
        { model | velocity = nextVelocity }


wallCollision : Model -> Model
wallCollision ({ bounds, velocity, movingCircle } as model) =
    let
        nextVelocity =
            WC.velocityAfterCollision bounds velocity movingCircle
    in
        { model | velocity = nextVelocity }


advanceCircle : Model -> Model
advanceCircle model =
    let
        nextMovingCircle =
            Circle.advance model.velocity model.movingCircle
    in
        { model | movingCircle = nextMovingCircle }


applyFriction : Model -> Model
applyFriction model =
    case Friction.apply model.velocity of
        Friction.SlowsDownCircle nextVelocity ->
            { model | velocity = nextVelocity }

        Friction.CausesStop ->
            { model
                | velocity = ( 0, 0 )
                , state = Growing
            }


growCircle : Model -> Model
growCircle ({ movingCircle, stationaryCircles, bounds } as model) =
    case Growth.grow movingCircle stationaryCircles bounds of
        Growth.Stopped ->
            let
                nextStationaryCircles =
                    movingCircle :: stationaryCircles

                nextMovingCircle =
                    Model.initialCircle
            in
                { model
                    | state = Waiting
                    , movingCircle = nextMovingCircle
                    , stationaryCircles = nextStationaryCircles
                }

        Growth.Active nextCircle ->
            { model | movingCircle = nextCircle }


wrapReturnType : Model -> ( Model, Cmd a )
wrapReturnType model =
    ( model, Cmd.none )
