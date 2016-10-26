module Main exposing (..)

import Model exposing (Model)
import Update exposing (..)
import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import CircularCollision exposing (Circle)


main : Program Never
main =
    App.program
        { init = ( Model.initial, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- VIEW


view : Model -> Html Msg
view { movingCircle, stationaryCircle, bounds } =
    svg [ viewBox (boundsToString bounds) ]
        [ circleToSvg movingCircle "#18a19a"
        , circleToSvg stationaryCircle "#0B79CE"
        ]


boundsToString : ( Int, Int ) -> String
boundsToString ( x, y ) =
    "0 0 " ++ (toString x) ++ " " ++ (toString y)


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
