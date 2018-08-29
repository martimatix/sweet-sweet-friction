module View exposing (view)

-- import TouchEvents exposing (emptyTouch, onTouchStart)

import Bounds
import CannonAngle
import Circle exposing (Circle)
import Html exposing (Html)
import Html.Events.Extra.Touch as Touch
import Model exposing (Model, State(..))
import RadialBurst exposing (RadialBurst)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        svgActiveCircle =
            circleToSvg model.activeCircle

        svgStationaryCircles =
            List.map circleToSvg model.stationaryCircles

        svgCircles =
            svgActiveCircle :: svgStationaryCircles

        svgRadialBursts =
            List.map svgRadialBurst model.radialBursts

        (( scoreX, scoreY ) as scorePosition) =
            ( 10, 10 )

        highScorePosition =
            ( Bounds.activeX - scoreX, scoreY )
    in
    svg
        [ viewBox gameBoundsToString
        , Touch.onStart UserInput
        , onClick (UserInput emptyTouch)
        , Svg.Attributes.style "background: black"
        , gameDimensions model.windowWidth model.windowHeight
        , class "noselect"
        ]
        [ svgCannonMargin
        , svgGameTitle model.backgroundTextOpacity
        , Svg.g [] svgCircles
        , Svg.g [] svgRadialBursts
        , svgCannon model.ticks
        , svgScore model.score scorePosition "Score" "start"
        , svgScore model.highScore highScorePosition "Hi-Score" "end"
        , svgCannonCover
        , svgGameOver model.state
        ]


gameDimensions : Int -> Int -> Svg.Attribute msg
gameDimensions windowWidth windowHeight =
    let
        gameRatio =
            toFloat Bounds.gameX / toFloat Bounds.gameY

        windowMargin =
            16

        width =
            windowWidth - windowMargin

        height =
            windowHeight - windowMargin

        windowRatio =
            toFloat width / toFloat height
    in
    if windowRatio > gameRatio then
        Svg.Attributes.height (String.fromInt height ++ "px")

    else
        Svg.Attributes.width (String.fromInt width ++ "px")


gameBoundsToString : String
gameBoundsToString =
    "0 0 " ++ String.fromInt Bounds.gameX ++ " " ++ String.fromInt Bounds.gameY


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
            toFloat Bounds.gameX / 2

        rotationPointY =
            toFloat (cannonY + cannonHeight)
    in
    Svg.rect
        [ x (String.fromInt cannonX)
        , y (String.fromInt cannonY)
        , width (String.fromInt cannonWidth)
        , height (String.fromInt cannonHeight)
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
            String.fromFloat angle

        x =
            String.fromFloat rotationPointX

        y =
            String.fromFloat rotationPointY
    in
    "rotate(" ++ angleString ++ " " ++ x ++ " " ++ y ++ ")"


circleToSvg : Circle -> Svg a
circleToSvg { cx, cy, radius, hitPoints, rotation } =
    Svg.g []
        [ Svg.circle
            [ Svg.Attributes.cx (String.fromFloat cx)
            , Svg.Attributes.cy (String.fromFloat cy)
            , r (String.fromFloat radius)
            , fill "white"
            ]
            []
        , Svg.text_
            [ x (String.fromFloat cx)
            , y (String.fromFloat cy)
            , fontFamily "Haettenschweiler"
            , fontSize (String.fromFloat (radius * 1.8))
            , textAnchor "middle"
            , dominantBaseline "central"
            , transform (rotateTransform (toFloat rotation) cx cy)
            ]
            [ Svg.text (String.fromInt hitPoints) ]
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
        , y1 (String.fromInt marginHeight)
        , x2 (String.fromInt Bounds.activeX)
        , y2 (String.fromInt marginHeight)
        , strokeDasharray "10, 5"
        , strokeWidth (String.fromInt lineThickness)
        , stroke "white"
        ]
        []


svgRadialBurst : RadialBurst -> Svg a
svgRadialBurst { cx, cy, radius, strokeWidth } =
    Svg.circle
        [ Svg.Attributes.cx (String.fromFloat cx)
        , Svg.Attributes.cy (String.fromFloat cy)
        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
        , stroke "white"
        , r (String.fromFloat radius)
        , fill "none"
        ]
        []


svgScore : Int -> ( Int, Int ) -> String -> String -> Svg a
svgScore score ( positionX, positionY ) scoreLabel anchor =
    let
        scoreText =
            scoreLabel ++ ": " ++ String.fromInt score
    in
    Svg.text_
        [ x (String.fromInt positionX)
        , y (String.fromInt (Bounds.gameY - positionY))
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
            [ Svg.Attributes.cx (String.fromFloat cx)
            , Svg.Attributes.cy (String.fromFloat cy)
            , r (String.fromInt radius)
            , fill "white"
            ]
            []
        , Svg.rect
            [ x (String.fromInt (Bounds.gameX // 2 - radius))
            , y (String.fromFloat cy)
            , width (String.fromInt (radius * 2))
            , height (String.fromInt (Bounds.gameY - round cy))
            , fill "white"
            ]
            []
        , Svg.text_
            [ x (String.fromInt (Bounds.gameX // 2))
            , y (String.fromInt (Bounds.gameY - 10))
            , fill "black"
            , fontFamily "Haettenschweiler"
            , fontSize "50"
            , textAnchor "middle"
            ]
            [ Svg.text "3210" ]
        ]


svgGameTitle : Float -> Svg a
svgGameTitle backgroundTextOpacity =
    Svg.text_
        [ y "150"
        , stroke "rgb(255, 163, 255)"
        , strokeWidth "1px"
        , fontFamily "Haettenschweiler"
        , fontSize "80"
        , strokeOpacity (String.fromFloat backgroundTextOpacity)
        ]
        (List.map svgGameTextLine
            [ "SWEET"
            , "SWEET"
            , "FRICTION"
            ]
        )


svgGameTextLine : String -> Svg a
svgGameTextLine text =
    let
        screenCentre =
            String.fromInt (Bounds.gameX // 2)
    in
    Svg.tspan
        [ dy "60"
        , x screenCentre
        , textAnchor "middle"
        ]
        [ Svg.text text ]


svgGameOver : State -> Svg a
svgGameOver state =
    case state of
        GameOver ->
            Svg.text_
                [ y "200"
                , fill "rgb(255, 163, 255)"
                , fontFamily "Haettenschweiler"
                , fontSize "80"
                , fillOpacity (String.fromInt 1)
                ]
                (List.map svgGameTextLine
                    [ "GAME"
                    , "OVER"
                    ]
                )

        _ ->
            Svg.g [] []


emptyTouch : Touch.Event
emptyTouch =
    let
        keys =
            { alt = False
            , ctrl = False
            , shift = False
            }
    in
    { keys = keys
    , changedTouches = []
    , targetTouches = []
    , touches = []
    }
