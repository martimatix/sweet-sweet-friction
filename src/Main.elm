module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import CircularCollision as CC exposing (Circle)
import Vector exposing (Vector)


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
    ( Model (Circle -20 50 50) (Circle 50 500 50) ( 0, -1 )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if model.movingCircle.cx > 500 then
                init
            else
                let
                    colliding =
                        CC.collision model.stationaryCircle model.movingCircle

                    velocity =
                        nextVelocity model colliding
                in
                    { model
                        | movingCircle = advanceCircle model.movingCircle velocity
                        , velocity = velocity
                    }
                        ! []


advanceCircle : Circle -> Vector -> Circle
advanceCircle circle ( x, y ) =
    Circle (circle.cx + x) (circle.cy + y) circle.radius


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- VIEW


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 500 500" ]
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
