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
view { movingCircle, stationaryCircles, bounds } =
    let
        svgMovingCircle =
            circleToSvg "#18a19a" movingCircle

        svgStationaryCircles =
            List.map (circleToSvg "#0B79CE") stationaryCircles

        svgCircles =
            svgMovingCircle :: svgStationaryCircles
    in
        svg [ viewBox (boundsToString bounds) ] svgCircles


boundsToString : ( Int, Int ) -> String
boundsToString ( x, y ) =
    "0 0 " ++ (toString x) ++ " " ++ (toString y)


circleToSvg : String -> Circle -> Svg a
circleToSvg fillColour circle =
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
