module View exposing (view)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Update exposing (Msg(FireCannon))
import Bounds exposing (Bounds)
import CannonAngle
import Circle exposing (Circle)


view : Model -> Html Msg
view { activeCircle, stationaryCircles, bounds, ticks, cannonMargin } =
    let
        svgActiveCircle =
            circleToSvg "#18a19a" activeCircle

        svgStationaryCircles =
            List.map (circleToSvg "#0B79CE") stationaryCircles

        svgCircles =
            svgActiveCircle :: svgStationaryCircles

        svgCannon =
            cannon bounds ticks
    in
        svg
            [ viewBox (boundsToString bounds)
            , onClick FireCannon
            , height "600px"
            , Svg.Attributes.style "background: yellow"
            ]
            [ g [] svgCircles
            , svgCannon
            , svgCannonMargin cannonMargin bounds
            ]


boundsToString : Bounds -> String
boundsToString ( x, y ) =
    "0 0 " ++ (toString x) ++ " " ++ (toString y)


cannon : Bounds -> Int -> Svg a
cannon ( boundsX, boundsY ) ticks =
    let
        initialCircle =
            Model.initialCircle

        cannonX =
            boundsX // 2 - cannonWidth // 2

        cannonY =
            round initialCircle.cy - cannonHeight

        cannonWidth =
            50

        cannonHeight =
            100

        angle =
            CannonAngle.ticksToSvgAngle ticks

        rotationPointX =
            boundsX // 2

        rotationPointY =
            cannonY + cannonHeight
    in
        Svg.rect
            [ x (toString cannonX)
            , y (toString cannonY)
            , width (toString cannonWidth)
            , height (toString cannonHeight)
            , fill "#000"
            , transform (cannonTransform angle rotationPointX rotationPointY)
            ]
            []


cannonTransform : Int -> Int -> Int -> String
cannonTransform angle rotationPointX rotationPointY =
    let
        angleString =
            toString angle

        x =
            toString rotationPointX

        y =
            toString rotationPointY
    in
        "rotate(" ++ angleString ++ " " ++ x ++ " " ++ y ++ ")"


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


svgCannonMargin : Int -> Bounds -> Svg a
svgCannonMargin margin ( boundsX, boundsY ) =
    let
        marginHeight =
            toString (boundsY - margin)
    in
        Svg.line
            [ x1 "0"
            , y1 marginHeight
            , x2 (toString boundsX)
            , y2 marginHeight
            , strokeDasharray "10, 5"
            , strokeWidth "5"
            , stroke "black"
            ]
            []
