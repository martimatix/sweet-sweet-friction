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
        ( velocityMagnitude * cos (angle |> degrees)
        , velocityMagnitude * sin (angle |> degrees) |> negate
        )


circularCollision : Model -> Model
circularCollision ({ activeCircle, stationaryCircles, velocity } as model) =
    let
        ( collidingCircles, otherCircles ) =
            CC.partitionCircles activeCircle stationaryCircles

        nextVelocity =
            CC.nextVelocity velocity activeCircle collidingCircles

        damagedCollidingCircles =
            collidingCircles
                |> List.map CC.applyDamage
                |> List.filter (\circle -> circle.hitPoints > 0)
    in
        { model
            | velocity = nextVelocity
            , stationaryCircles = damagedCollidingCircles ++ otherCircles
        }


wallCollision : Model -> Model
wallCollision ({ bounds, velocity, activeCircle } as model) =
    let
        nextVelocity =
            WC.velocityAfterCollision bounds velocity activeCircle
    in
        { model | velocity = nextVelocity }


advanceCircle : Model -> Model
advanceCircle model =
    let
        nextActiveCircle =
            Circle.advance model.velocity model.activeCircle
    in
        { model | activeCircle = nextActiveCircle }


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
growCircle ({ activeCircle, stationaryCircles, bounds } as model) =
    case Growth.grow activeCircle stationaryCircles bounds of
        Growth.Stopped ->
            let
                nextStationaryCircles =
                    activeCircle :: stationaryCircles

                nextActiveCircle =
                    Model.initialCircle
            in
                { model
                    | state = Waiting
                    , activeCircle = nextActiveCircle
                    , stationaryCircles = nextStationaryCircles
                }

        Growth.Active biggerActiveCircle ->
            { model | activeCircle = biggerActiveCircle }


wrapReturnType : Model -> ( Model, Cmd a )
wrapReturnType model =
    ( model, Cmd.none )
