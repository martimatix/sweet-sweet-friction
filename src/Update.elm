module Update exposing (update, Msg(..))

import Model exposing (..)
import Circle exposing (Circle)
import Circle.Collision as CC
import Circle.Growth as CG
import WallCollision as WC
import Vector exposing (Vector)
import Friction
import CannonAngle


type Msg
    = Tick Float
    | FireCannon


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            model
                |> applyFriction
                |> circularCollision
                |> wallCollision
                |> advanceCircle
                |> growCircle
                |> incrementTick
                |> wrapReturnType

        FireCannon ->
            let
                nextVelocity =
                    initialVelocity model.ticks
            in
                { model
                    | velocity = nextVelocity
                    , movingCircle = Circle 175 500 15
                }
                    ! []


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


applyFriction : Model -> Model
applyFriction model =
    { model | velocity = Friction.apply model.velocity }


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


growCircle : Model -> Model
growCircle ({ movingCircle, stationaryCircles, bounds } as model) =
    if Vector.magnitude model.velocity == 0 then
        { model
            | movingCircle = CG.grow movingCircle stationaryCircles bounds
        }
    else
        model


incrementTick : Model -> Model
incrementTick model =
    { model | ticks = model.ticks + 1 }


wrapReturnType : Model -> ( Model, Cmd a )
wrapReturnType model =
    ( model, Cmd.none )
