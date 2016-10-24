module Update exposing (update, Msg(..))

import Model exposing (..)
import CircularCollision as CC exposing (Circle)
import Vector exposing (Vector)


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            model
                |> checkOutOfBounds
                |> applyFriction
                |> circularCollision
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


checkOutOfBounds : Model -> Model
checkOutOfBounds model =
    if model.movingCircle.cx > 500 then
        Model.initial
    else
        model


nextVelocity : Model -> Bool -> Vector
nextVelocity model colliding =
    if colliding then
        let
            stationaryCircle =
                model.stationaryCircle

            movingCircle =
                model.movingCircle

            velocity =
                model.velocity
        in
            CC.velocityAfterCollision stationaryCircle movingCircle velocity
    else
        model.velocity


circularCollision : Model -> Model
circularCollision model =
    let
        colliding =
            CC.collision model.stationaryCircle model.movingCircle
    in
        { model | velocity = nextVelocity model colliding }


advanceCircle : Model -> Model
advanceCircle model =
    let
        ( x, y ) =
            model.velocity

        circle =
            model.movingCircle
    in
        { model | movingCircle = Circle (circle.cx + x) (circle.cy + y) circle.radius }


wrapReturnType : Model -> ( Model, Cmd a )
wrapReturnType model =
    ( model, Cmd.none )
