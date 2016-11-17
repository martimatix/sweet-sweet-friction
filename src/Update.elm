module Update exposing (update, Msg(..))

import Model exposing (..)
import Circle exposing (Circle)
import Circle.Collision as CC
import Circle.Growth as Growth
import WallCollision as WC
import Vector exposing (Vector)
import Friction exposing (Result(..))
import Bounds
import CannonAngle


type Msg
    = Tick Float
    | FireCannon


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( animate model, Cmd.none )

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
                |> checkGameOver

        Growing targetRadius ->
            growCircle targetRadius model

        GameOver ->
            model


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
            8
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
wallCollision ({ velocity, activeCircle } as model) =
    let
        nextVelocity =
            WC.velocityAfterCollision Bounds.game velocity activeCircle
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
applyFriction ({ activeCircle, stationaryCircles } as model) =
    case Friction.apply model.velocity of
        Friction.SlowsDownCircle nextVelocity ->
            { model | velocity = nextVelocity }

        Friction.CausesStop ->
            { model
                | velocity = ( 0, 0 )
                , state = Growing (Growth.targetRadius activeCircle stationaryCircles)
            }


growCircle : Float -> Model -> Model
growCircle targetRadius ({ activeCircle, stationaryCircles, ticks } as model) =
    if model.growTicks == 50 then
        { model
            | state = Waiting
            , activeCircle = Model.initialCircle (ticks % 30 - 15)
            , stationaryCircles = activeCircle :: stationaryCircles
            , growTicks = 0
        }
    else
        { model
            | activeCircle = Growth.nextCircle targetRadius activeCircle
            , growTicks = model.growTicks + 1
        }


checkGameOver : Model -> Model
checkGameOver ({ velocity, activeCircle } as model) =
    let
        ( velocityX, velocityY ) =
            velocity

        circleTravellingDownwards =
            velocityY > 0

        ( _, boundsY ) =
            Bounds.active

        circleOutsideActiveBounds =
            activeCircle.cy + activeCircle.radius > toFloat boundsY
    in
        if circleTravellingDownwards && circleOutsideActiveBounds then
            { model | state = GameOver }
        else
            model
