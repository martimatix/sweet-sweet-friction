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
import RadialBurst


type Msg
    = Tick Float
    | FireCannon
    | NewGame
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( animate model, Cmd.none )

        FireCannon ->
            { model
                | state = Travelling
                , velocity = initialVelocity model.ticks
            }
                ! []

        NewGame ->
            let
                nextModel =
                    Model.initial
            in
                { nextModel | highScore = model.highScore } ! []

        NoOp ->
            model ! []


animate : Model -> Model
animate model =
    model
        |> radialBurst
        |> animateState


animateState : Model -> Model
animateState model =
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

        Growing growthIncrement growTicks ->
            growCircle growthIncrement growTicks model

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

        ( damagedCollidingCircles, deadCircles ) =
            collidingCircles
                |> List.map CC.applyDamage
                |> List.partition (\circle -> circle.hitPoints > 0)

        newRadialBursts =
            List.map RadialBurst.create deadCircles

        nextScore =
            List.length deadCircles + model.score
    in
        { model
            | velocity = nextVelocity
            , stationaryCircles = damagedCollidingCircles ++ otherCircles
            , radialBursts = newRadialBursts ++ model.radialBursts
            , score = nextScore
            , highScore = max nextScore model.highScore
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
            let
                growthIncrement =
                    Growth.growthIncrement activeCircle stationaryCircles

                growthTicks =
                    Growth.ticksToFullSize
            in
                { model
                    | velocity = ( 0, 0 )
                    , state = Growing growthIncrement growthTicks
                }


initialiseNextTurn : Model -> Model
initialiseNextTurn ({ activeCircle, stationaryCircles, ticks } as model) =
    let
        randomRotation =
            ticks % 30 - 15
    in
        { model
            | state = Waiting
            , activeCircle = Model.initialCircle randomRotation
            , stationaryCircles = activeCircle :: stationaryCircles
        }


increaseActiveCircleRadius : Float -> Int -> Model -> Model
increaseActiveCircleRadius growthIncrement growTicks ({ activeCircle } as model) =
    let
        nextCircle =
            { activeCircle | radius = activeCircle.radius + growthIncrement }
    in
        { model
            | activeCircle = nextCircle
            , state = Growing growthIncrement (growTicks - 1)
        }


growCircle : Float -> Int -> Model -> Model
growCircle growthIncrement growTicks model =
    if growTicks == 0 then
        initialiseNextTurn model
    else
        increaseActiveCircleRadius growthIncrement growTicks model


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


radialBurst : Model -> Model
radialBurst ({ radialBursts } as model) =
    let
        nextRadialBursts =
            radialBursts
                |> List.map RadialBurst.advance
                |> List.filter RadialBurst.visible
    in
        { model | radialBursts = nextRadialBursts }
