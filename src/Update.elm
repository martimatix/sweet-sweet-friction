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
import RadialBurst exposing (RadialBurst)
import LocalStorage
import Task exposing (Task)
import Window
import TouchEvents


type Msg
    = Init
    | Tick Float
    | UserInput TouchEvents.Touch
    | Load String
    | WindowResize Window.Size
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            ( model, getFromStorage )

        Tick _ ->
            model
                |> incrementDebounceTick
                |> radialBurst
                |> animateState
                |> saveToStorage

        UserInput touch ->
            debounceAction model

        Load highScore ->
            let
                nextHighScore =
                    Result.withDefault 0 (String.toInt highScore)
            in
                { model | highScore = nextHighScore } ! []

        WindowResize windowSize ->
            { model
                | windowWidth = windowSize.width
                , windowHeight = windowSize.height
            }
                ! []

        NoOp ->
            model ! []


incrementDebounceTick : Model -> Model
incrementDebounceTick model =
    { model | debounceTicks = model.debounceTicks + 1 }


debounceAction : Model -> ( Model, Cmd msg )
debounceAction model =
    let
        ticksUntilNextActionPermitted =
            20

        nextActionPermitted =
            ticksUntilNextActionPermitted <= model.debounceTicks
    in
        if nextActionPermitted then
            action { model | debounceTicks = 0 }
        else
            model ! []


action : Model -> ( Model, Cmd msg )
action model =
    case model.state of
        Waiting ->
            fireCannon model

        GameOver ->
            newGame model

        _ ->
            model ! []


newGame : Model -> ( Model, Cmd msg )
newGame model =
    let
        initialModel =
            Model.initial

        nextRadialBursts =
            burstStationaryCircles model.stationaryCircles
    in
        { initialModel
            | highScore = model.highScore
            , windowWidth = model.windowWidth
            , windowHeight = model.windowHeight
            , backgroundTextOpacity = 0
            , radialBursts = nextRadialBursts
        }
            ! []


fireCannon : Model -> ( Model, Cmd msg )
fireCannon model =
    { model
        | state = Travelling
        , velocity = initialVelocity model.ticks
    }
        ! []


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
                |> fadeOutBackgroundText
                |> checkGameOver

        Growing growthIncrement growTicks ->
            growCircle growthIncrement growTicks model

        GameOver ->
            model
                |> burstActiveCircle


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
wallCollision ({ activeCircle, velocity } as model) =
    let
        ( nextActiveCircle, nextVelocity ) =
            WC.collision ( activeCircle, velocity )
    in
        { model
            | activeCircle = nextActiveCircle
            , velocity = nextVelocity
        }


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


fadeOutBackgroundText : Model -> Model
fadeOutBackgroundText ({ backgroundTextOpacity } as model) =
    let
        fadeRate =
            0.01

        nextBackgroundTextOpacity =
            max 0 (backgroundTextOpacity - fadeRate)
    in
        { model | backgroundTextOpacity = nextBackgroundTextOpacity }


checkGameOver : Model -> Model
checkGameOver ({ velocity, activeCircle } as model) =
    let
        ( velocityX, velocityY ) =
            velocity

        circleTravellingDownwards =
            velocityY > 0

        circleOutsideActiveBounds =
            activeCircle.cy + activeCircle.radius > toFloat Bounds.activeY
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


burstStationaryCircles : List Circle -> List RadialBurst
burstStationaryCircles stationaryCircles =
    List.map RadialBurst.create stationaryCircles


burstActiveCircle : Model -> Model
burstActiveCircle ({ activeCircle, radialBursts } as model) =
    let
        burstActiveCircle =
            RadialBurst.create activeCircle

        nextRadialBursts =
            burstActiveCircle :: radialBursts
    in
        { model
            | radialBursts = nextRadialBursts
            , activeCircle = Model.initialCircle 0
        }


getFromStorage : Cmd Msg
getFromStorage =
    LocalStorage.get "sweet-sweet-friction"
        |> Task.attempt
            (\result ->
                case result of
                    Ok v ->
                        Load (Maybe.withDefault "0" v)

                    Err _ ->
                        Load "0"
            )


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    LocalStorage.set "sweet-sweet-friction" (toString model.highScore)
        |> Task.attempt (always NoOp)
        |> (,) model
