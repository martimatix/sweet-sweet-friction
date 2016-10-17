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
    { fixedCircle : Circle
    , dynamicCircle : Circle
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Circle 50 0 45) (Circle 50 0 45)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if model.dynamicCircle.cy > 200 then
                init
            else
                { model | dynamicCircle = advanceCircle model.dynamicCircle } ! []


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
        [ myCircle model
        ]


myCircle : Model -> Svg a
myCircle model =
    let
        y =
            circleYPosition model.dynamicCircle
    in
        circle [ cx "50", cy y, r "45", fill "#0B79CE" ] []


circleYPosition : Circle -> String
circleYPosition circle =
    toString (circle.cy * -3 + 500)
