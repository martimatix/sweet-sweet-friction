module Update exposing (update, Msg(..))

import Model exposing (..)
import Circle
import Circle.Collision as CC
import Circle.Growth as CG
import WallCollision as WC
import Vector exposing (Vector)
import Friction


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
                |> growCircle
                |> wrapReturnType


applyFriction : Model -> Model
applyFriction model =
    { model | velocity = Friction.apply model.velocity }


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


growCircle : Model -> Model
growCircle model =
    let
        { movingCircle, stationaryCircles, bounds } =
            model
    in
        if Vector.magnitude model.velocity == 0 then
            { model
                | movingCircle = CG.grow movingCircle stationaryCircles bounds
            }
        else
            model


wrapReturnType : Model -> ( Model, Cmd a )
wrapReturnType model =
    ( model, Cmd.none )
