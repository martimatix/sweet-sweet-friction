module View exposing (view)

import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Model exposing (Model, State(..))
import Update exposing (Msg(..))
import Bounds
import CannonAngle
import Circle exposing (Circle)
import RadialBurst exposing (RadialBurst)


view : Model -> Html Msg
view { activeCircle, stationaryCircles, ticks, radialBursts, score, highScore, state, windowHeight } =
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
            ( Bounds.activeX - scoreX, scoreY )

        windowMargin =
            16
    in
        svg
            [ viewBox gameBoundsToString
            , onClick (clickEvent state)
            , height ((toString (windowHeight - windowMargin)) ++ "px")
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


gameBoundsToString : String
gameBoundsToString =
    "0 0 " ++ (toString Bounds.gameX) ++ " " ++ (toString Bounds.gameY)


svgCannon : Int -> Svg a
svgCannon ticks =
    let
        initialCircle =
            Model.initialCircle 0

        cannonX =
            Bounds.gameX // 2 - cannonWidth // 2

        cannonY =
            round initialCircle.cy - cannonHeight

        cannonWidth =
            32

        cannonHeight =
            81

        angle =
            CannonAngle.ticksToSvgAngle ticks

        rotationPointX =
            (toFloat Bounds.gameX) / 2

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
        lineThickness =
            2

        marginHeight =
            Bounds.activeY + lineThickness // 2
    in
        Svg.line
            [ x1 "0"
            , y1 (toString marginHeight)
            , x2 (toString Bounds.activeX)
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
        scoreText =
            scoreLabel ++ ": " ++ (toString score)
    in
        Svg.text_
            [ x (toString positionX)
            , y (toString (Bounds.gameY - positionY))
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
                [ x (toString (Bounds.gameX // 2 - radius))
                , y (toString cy)
                , width (toString (radius * 2))
                , height (toString (Bounds.gameY - (round cy)))
                , fill "white"
                ]
                []
            , Svg.text_
                [ x (toString (Bounds.gameX // 2))
                , y (toString (Bounds.gameY - 10))
                , fill "black"
                , fontFamily "Haettenschweiler"
                , fontSize "50"
                , textAnchor "middle"
                ]
                [ Svg.text "3210" ]
            ]
