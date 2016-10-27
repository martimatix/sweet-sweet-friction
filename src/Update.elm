module Update exposing (update, Msg(..))

import Model exposing (..)
import Circle
import Circle.Collision as CC
import WallCollision as WC
import Vector exposing (Vector)


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            model
                |> applyFriction
                |> circularCollision
                |> wallCollision
                |> advanceCircle
                |> wrapReturnType


applyFriction : Model -> Model
applyFriction model =
    let
        frictionMagnitude =
            0.001

        frictionVector =
            model.velocity
                |> Vector.normalise
                |> Vector.negate
                |> Vector.scale frictionMagnitude

        nextVelocity =
            Vector.add model.velocity frictionVector
    in
        { model | velocity = nextVelocity }


circularCollision : Model -> Model
circularCollision model =
    let
        { velocity, movingCircle, stationaryCircles } =
            model

        nextVelocity =
            CC.nextVelocity velocity movingCircle stationaryCircles
    in
        { model | velocity = nextVelocity }


wallCollision : Model -> Model
wallCollision model =
    let
        { bounds, velocity, movingCircle } =
            model

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


wrapReturnType : Model -> ( Model, Cmd a )
wrapReturnType model =
    ( model, Cmd.none )
