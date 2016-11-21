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
import RadialBurst exposing (RadialBurst)


view : Model -> Html Msg
view { activeCircle, stationaryCircles, ticks, radialBursts } =
    let
        svgActiveCircle =
            circleToSvg activeCircle

        svgStationaryCircles =
            List.map circleToSvg stationaryCircles

        svgCircles =
            svgActiveCircle :: svgStationaryCircles

        svgCannon =
            cannon ticks

        svgRadialBursts =
            List.map svgRadialBurst radialBursts
    in
        svg
            [ viewBox (boundsToString Bounds.game)
            , onClick FireCannon
            , height "600px"
            , Svg.Attributes.style "background: black"
            ]
            [ g [] svgCircles
            , g [] svgRadialBursts
            , svgCannon
            , svgCannonMargin
            ]


boundsToString : Bounds -> String
boundsToString ( x, y ) =
    "0 0 " ++ (toString x) ++ " " ++ (toString y)


cannon : Int -> Svg a
cannon ticks =
    let
        ( boundsX, boundsY ) =
            Bounds.game

        initialCircle =
            Model.initialCircle 0

        cannonX =
            boundsX // 2 - cannonWidth // 2

        cannonY =
            round initialCircle.cy - cannonHeight

        cannonWidth =
            36

        cannonHeight =
            85

        angle =
            CannonAngle.ticksToSvgAngle ticks

        rotationPointX =
            (toFloat boundsX) / 2

        rotationPointY =
            toFloat (cannonY + cannonHeight)
    in
        Svg.rect
            [ x (toString cannonX)
            , y (toString cannonY)
            , width (toString cannonWidth)
            , height (toString cannonHeight)
            , fill "white"
            , transform (rotateTransform angle rotationPointX rotationPointY)
            ]
            []


rotateTransform : Float -> Float -> Float -> String
rotateTransform angle rotationPointX rotationPointY =
    let
        angleString =
            toString angle

        x =
            toString rotationPointX

        y =
            toString rotationPointY
    in
        "rotate(" ++ angleString ++ " " ++ x ++ " " ++ y ++ ")"


circleToSvg : Circle -> Svg a
circleToSvg { cx, cy, radius, hitPoints, rotation } =
    g []
        [ Svg.circle
            [ Svg.Attributes.cx (toString cx)
            , Svg.Attributes.cy (toString cy)
            , r (toString radius)
            , fill "white"
            ]
            []
        , Svg.text_
            [ x (toString cx)
            , y (toString (cy - radius * 0.05))
            , fontFamily "Haettenschweiler"
            , fontSize (toString (radius * 1.8))
            , textAnchor "middle"
            , dominantBaseline "central"
            , transform (rotateTransform (toFloat rotation) cx cy)
            ]
            [ text (toString hitPoints) ]
        ]


svgCannonMargin : Svg a
svgCannonMargin =
    let
        ( boundsX, boundsY ) =
            Bounds.active

        lineThickness =
            2

        marginHeight =
            boundsY + lineThickness // 2
    in
        Svg.line
            [ x1 "0"
            , y1 (toString marginHeight)
            , x2 (toString boundsX)
            , y2 (toString marginHeight)
            , strokeDasharray "10, 5"
            , strokeWidth (toString lineThickness)
            , stroke "white"
            ]
            []


svgRadialBurst : RadialBurst -> Svg a
svgRadialBurst { cx, cy, radius, strokeWidth } =
    Svg.circle
        [ Svg.Attributes.cx (toString cx)
        , Svg.Attributes.cy (toString cy)
        , Svg.Attributes.strokeWidth (toString strokeWidth)
        , stroke "white"
        , r (toString radius)
        , fill "none"
        ]
        []
