module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import CircularCollision as CC exposing (Circle, Vector)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { stationaryCircle : Circle
    , movingCircle : Circle
    , velocity : Vector
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Circle 30 100 20) (Circle 50 500 45) ( 0, -2 )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if model.movingCircle.cy < (0 - model.movingCircle.radius) then
                init
            else
                let
                    colliding =
                        CC.collision model.stationaryCircle model.movingCircle
                in
                    { model
                        | movingCircle = nextPosition model colliding
                        , velocity = nextVelocity model colliding
                    }
                        ! []


nextPosition : Model -> Bool -> Circle
nextPosition model colliding =
    if colliding then
        CC.circleAtCollision model.stationaryCircle model.movingCircle model.velocity
    else
        advanceCircle model.movingCircle model.velocity


advanceCircle : Circle -> Vector -> Circle
advanceCircle circle ( x, y ) =
    Circle (circle.cx + x) (circle.cy + y) circle.radius


nextVelocity : Model -> Bool -> Vector
nextVelocity model colliding =
    let
        stationaryCircle =
            model.stationaryCircle

        movingCircle =
            model.movingCircle

        velocity =
            model.velocity
    in
        if colliding then
            CC.velocityAfterCollision stationaryCircle movingCircle velocity
        else
            model.velocity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- VIEW


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 100 500", width "300px" ]
        [ circleToSvg model.movingCircle "#18a19a"
        , circleToSvg model.stationaryCircle "#0B79CE"
        ]


circleToSvg : Circle -> String -> Svg a
circleToSvg circle fillColour =
    let
        xCentre =
            toString circle.cx

        yCentre =
            toString circle.cy

        radius =
            toString circle.radius
    in
        Svg.circle
            [ cx xCentre
            , cy yCentre
            , r radius
            , fill fillColour
            ]
            []
