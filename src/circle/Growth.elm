module Circle.Growth exposing (grow, State(..))

import Circle exposing (Circle)
import Circle.Collision as CC
import WallCollision as WC
import Bounds exposing (Bounds)
import Vector


type alias GrowthModel =
    { collision : Bool
    , activeCircle : Circle
    , stationaryCircles : List Circle
    }


type State
    = Active Circle
    | Stopped


grownCircleRadius : Circle -> List Circle -> Bounds -> Maybe Float
grownCircleRadius activeCircle stationaryCircles ( boundsX, boundsY ) =
    let
        distanceToCircleEdges =
            List.map (distanceToCircleEdge activeCircle) stationaryCircles

        distanceToWalls =
            [ activeCircle.cx
            , activeCircle.cy
            , (toFloat boundsX) - activeCircle.cx
            , (toFloat boundsY) - activeCircle.cy
            ]
    in
        List.maximum (distanceToCircleEdges ++ distanceToWalls)


distanceToCircleEdge : Circle -> Circle -> Float
distanceToCircleEdge activeCircle stationaryCircle =
    let
        distanceBetweenCircles =
            Circle.vectorBetweenCentres activeCircle stationaryCircle
                |> Vector.magnitude
    in
        distanceBetweenCircles - activeCircle.radius


grow : Circle -> List Circle -> State
grow activeCircle stationaryCircles =
    growthModel activeCircle stationaryCircles
        |> checkForCircularCollision
        |> checkForWallCollision
        |> applyGrowth


growthModel : Circle -> List Circle -> GrowthModel
growthModel =
    GrowthModel False


checkForCircularCollision : GrowthModel -> GrowthModel
checkForCircularCollision ({ activeCircle, stationaryCircles } as model) =
    { model | collision = CC.anyCollisions activeCircle stationaryCircles }


checkForWallCollision : GrowthModel -> GrowthModel
checkForWallCollision ({ activeCircle } as model) =
    let
        wallCollision =
            WC.collision Bounds.active activeCircle
    in
        { model | collision = model.collision || wallCollision }


applyGrowth : GrowthModel -> State
applyGrowth { collision, activeCircle } =
    if collision then
        Stopped
    else
        Active { activeCircle | radius = activeCircle.radius + 0.5 }
