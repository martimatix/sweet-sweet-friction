module Circle.Growth exposing (targetRadius, nextCircle)

import Model
import Circle exposing (Circle)
import Bounds exposing (Bounds)


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


nextCircle : Float -> Circle -> Circle
nextCircle radiusTarget activeCircle =
    let
        ticksToGrowFullSize =
            50

        increment =
            (radiusTarget - Model.initialRadius) / ticksToGrowFullSize
    in
        { activeCircle | radius = activeCircle.radius + increment }
