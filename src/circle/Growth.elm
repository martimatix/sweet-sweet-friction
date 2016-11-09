module Circle.Growth exposing (grow, State(..))

import Circle exposing (Circle)
import Circle.Collision as CC
import WallCollision as WC


type alias GrowthModel =
    { collision : Bool
    , activeCircle : Circle
    , stationaryCircles : List Circle
    }


type State
    = Active Circle
    | Stopped


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
            WC.collision activeCircle
    in
        { model | collision = model.collision || wallCollision }


applyGrowth : GrowthModel -> State
applyGrowth { collision, activeCircle } =
    if collision then
        Stopped
    else
        Active { activeCircle | radius = activeCircle.radius + 0.5 }
