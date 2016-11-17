module Circle.Growth exposing (growthIncrement, ticksToFullSize)

import Model
import Circle exposing (Circle)
import Bounds exposing (Bounds)


growthIncrement : Circle -> List Circle -> Float
growthIncrement activeCircle stationaryCircles =
    let
        distanceToGrow =
            targetRadius activeCircle stationaryCircles - Model.initialRadius
    in
        distanceToGrow / toFloat ticksToFullSize


targetRadius : Circle -> List Circle -> Float
targetRadius activeCircle stationaryCircles =
    let
        ( boundsX, boundsY ) =
            Bounds.active

        distanceToCircleEdges =
            List.map (distanceToCircleEdge activeCircle) stationaryCircles

        distanceToWalls =
            [ activeCircle.cx
            , activeCircle.cy
            , (toFloat boundsX) - activeCircle.cx
            , (toFloat boundsY) - activeCircle.cy
            ]
    in
        List.minimum (distanceToCircleEdges ++ distanceToWalls)
            |> Maybe.withDefault Model.initialRadius


distanceToCircleEdge : Circle -> Circle -> Float
distanceToCircleEdge activeCircle stationaryCircle =
    let
        distanceBetweenCircles =
            Circle.distanceBetweenCentres activeCircle stationaryCircle
    in
        distanceBetweenCircles - stationaryCircle.radius


ticksToFullSize : Int
ticksToFullSize =
    50
