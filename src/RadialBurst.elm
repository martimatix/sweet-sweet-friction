module RadialBurst exposing (RadialBurst, advance, create, visible)

import Circle exposing (Circle)


type alias RadialBurst =
    { cx : Float
    , cy : Float
    , radius : Float
    , strokeWidth : Float
    }


create : Circle -> RadialBurst
create { cx, cy, radius } =
    { cx = cx
    , cy = cy
    , radius = radius
    , strokeWidth = radius * 0.5
    }


advance : RadialBurst -> RadialBurst
advance ({ radius, strokeWidth } as radialBurst) =
    { radialBurst
        | radius = radius * 1.04
        , strokeWidth = strokeWidth * 0.7
    }


visible : RadialBurst -> Bool
visible radialBurst =
    radialBurst.strokeWidth > 0.1
