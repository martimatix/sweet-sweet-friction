module Main exposing (..)

import Model exposing (Model)
import Update exposing (..)
import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Circle exposing (Circle)
import CannonAngle


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
view { activeCircle, stationaryCircles, bounds, ticks } =
    let
        svgActiveCircle =
            circleToSvg "#18a19a" activeCircle

        svgStationaryCircles =
            List.map (circleToSvg "#0B79CE") stationaryCircles

        svgCircles =
            svgActiveCircle :: svgStationaryCircles

        svgCannon =
            cannon ticks
    in
        svg
            [ viewBox (boundsToString bounds)
            , onClick FireCannon
            , height "600px"
            ]
            (svgCircles ++ svgCannon)


boundsToString : ( Int, Int ) -> String
boundsToString ( x, y ) =
    "0 0 " ++ (toString x) ++ " " ++ (toString y)



-- TODO: This should not be a list


cannon : Int -> List (Svg a)
cannon ticks =
    let
        angle =
            CannonAngle.ticksToSvgAngle ticks
    in
        [ Svg.rect
            [ x "150"
            , y "400"
            , width "50"
            , height "100"
            , fill "#000"
            , transform ("rotate(" ++ (toString angle) ++ " 175 500)")
            ]
            []
        ]


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
