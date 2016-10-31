module Circle.Growth exposing (grow)

import Circle exposing (Circle)
import Circle.Collision as CC
import Bounds exposing (Bounds)
import WallCollision as WC


type alias GrowthModel =
    { collision : Bool
    , movingCircle : Circle
    , stationaryCircles : List Circle
    , bounds : Bounds
    }


grow : Circle -> List Circle -> Bounds -> Circle
grow movingCircle stationaryCircles bounds =
    growthModel movingCircle stationaryCircles bounds
        |> checkForCircularCollision
        |> checkForWallCollision
        |> applyGrowth


growthModel : Circle -> List Circle -> Bounds -> GrowthModel
growthModel =
    GrowthModel False


checkForCircularCollision : GrowthModel -> GrowthModel
checkForCircularCollision ({ movingCircle, stationaryCircles } as model) =
    let
        circularCollision =
            CC.collisionCircle movingCircle stationaryCircles
    in
        case circularCollision of
            Nothing ->
                { model | collision = model.collision || False }

            _ ->
                { model | collision = True }


checkForWallCollision : GrowthModel -> GrowthModel
checkForWallCollision ({ bounds, movingCircle } as model) =
    let
        wallCollision =
            WC.collision bounds movingCircle
    in
        { model | collision = model.collision || wallCollision }


applyGrowth : GrowthModel -> Circle
applyGrowth { collision, movingCircle } =
    if collision then
        movingCircle
    else
        { movingCircle | radius = movingCircle.radius + 1 }
