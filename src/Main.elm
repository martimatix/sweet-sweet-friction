module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import CircularCollision as CC exposing (Circle)


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
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Circle 30 30 20) (Circle 50 0 45)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if model.movingCircle.cy > 200 then
                init
            else
                { model | movingCircle = advanceCircle model.movingCircle } ! []


advanceCircle : Circle -> Circle
advanceCircle circle =
    Circle circle.cx (circle.cy + 1) circle.radius



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- VIEW


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 100 500", width "300px" ]
        [ movingCircle model.movingCircle
        , circleToSvg model.stationaryCircle "#0B79CE"
        ]


movingCircle : Circle -> Svg a
movingCircle circle =
    let
        circleYPosition =
            circle.cy * -3 + 500
    in
        circleToSvg { circle | cy = circleYPosition } "#89fb5e"


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
        Svg.circle [ cx xCentre, cy yCentre, r radius, fill fillColour ] []
