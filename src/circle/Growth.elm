module Circle.Growth exposing (grow, State(..))

import Circle exposing (Circle)
import Circle.Collision as CC
import Bounds exposing (Bounds)
import WallCollision as WC


type alias GrowthModel =
    { collision : Bool
    , activeCircle : Circle
    , stationaryCircles : List Circle
    , bounds : Bounds
    }


type State
    = Active Circle
    | Stopped


grow : Circle -> List Circle -> Bounds -> State
grow activeCircle stationaryCircles bounds =
    growthModel activeCircle stationaryCircles bounds
        |> checkForCircularCollision
        |> checkForWallCollision
        |> applyGrowth


growthModel : Circle -> List Circle -> Bounds -> GrowthModel
growthModel =
    GrowthModel False


checkForCircularCollision : GrowthModel -> GrowthModel
checkForCircularCollision ({ activeCircle, stationaryCircles } as model) =
    { model | collision = CC.anyCollisions activeCircle stationaryCircles }


checkForWallCollision : GrowthModel -> GrowthModel
checkForWallCollision ({ bounds, activeCircle } as model) =
    let
        wallCollision =
            WC.collision bounds activeCircle
    in
        { model | collision = model.collision || wallCollision }


applyGrowth : GrowthModel -> State
applyGrowth { collision, activeCircle } =
    if collision then
        Stopped
    else
        Active { activeCircle | radius = activeCircle.radius + 1 }
