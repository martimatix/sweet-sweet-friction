module View exposing (view)

import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Model exposing (Model, State(..))
import Update exposing (Msg(..))
import Bounds exposing (Bounds)
import CannonAngle
import Circle exposing (Circle)
import RadialBurst exposing (RadialBurst)


view : Model -> Html Msg
view { activeCircle, stationaryCircles, ticks, radialBursts, score, highScore, state } =
    let
        svgActiveCircle =
            circleToSvg activeCircle

        svgStationaryCircles =
            List.map circleToSvg stationaryCircles

        svgCircles =
            svgActiveCircle :: svgStationaryCircles

        svgRadialBursts =
            List.map svgRadialBurst radialBursts

        (( scoreX, scoreY ) as scorePosition) =
            ( 10, 10 )

        highScorePosition =
            ( (Tuple.first Bounds.active) - scoreX, scoreY )
    in
        svg
            [ viewBox (boundsToString Bounds.game)
            , onClick (clickEvent state)
            , height "600px"
            , Svg.Attributes.style "background: black"
            ]
            [ svgCannonMargin
            , Svg.g [] svgCircles
            , Svg.g [] svgRadialBursts
            , svgCannon ticks
            , svgScore score scorePosition "Score" "start"
            , svgScore highScore highScorePosition "Hi-Score" "end"
            , svgCannonCover
            ]


clickEvent : State -> Msg
clickEvent state =
    case state of
        Waiting ->
            FireCannon

        GameOver ->
            NewGame

        _ ->
            NoOp


boundsToString : Bounds -> String
boundsToString ( x, y ) =
    "0 0 " ++ (toString x) ++ " " ++ (toString y)


svgCannon : Int -> Svg a
svgCannon ticks =
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
            32

        cannonHeight =
            81

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
            , fill "black"
            , stroke "white"
            , strokeWidth "2"
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
    Svg.g []
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
            [ Svg.text (toString hitPoints) ]
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


svgScore : Int -> ( Int, Int ) -> String -> String -> Svg a
svgScore score ( positionX, positionY ) scoreLabel anchor =
    let
        ( _, boundsY ) =
            Bounds.game

        scoreText =
            scoreLabel ++ ": " ++ (toString score)
    in
        Svg.text_
            [ x (toString positionX)
            , y (toString (boundsY - positionY))
            , fontFamily "Haettenschweiler"
            , fontSize "30"
            , fill "white"
            , textAnchor anchor
            ]
            [ Svg.text scoreText ]


svgCannonCover : Svg a
svgCannonCover =
    let
        { cx, cy } =
            Model.initialCircle 0

        radius =
            50

        ( boundsX, boundsY ) =
            Bounds.game
    in
        Svg.g []
            [ Svg.circle
                [ Svg.Attributes.cx (toString cx)
                , Svg.Attributes.cy (toString cy)
                , r (toString radius)
                , fill "white"
                ]
                []
            , Svg.rect
                [ x (toString (boundsX // 2 - radius))
                , y (toString cy)
                , width (toString (radius * 2))
                , height (toString (boundsY - (round cy)))
                , fill "white"
                ]
                []
            , Svg.text_
                [ x (toString (boundsX // 2))
                , y (toString (boundsY - 10))
                , fill "black"
                , fontFamily "Haettenschweiler"
                , fontSize "50"
                , textAnchor "middle"
                ]
                [ Svg.text "3210" ]
            ]
