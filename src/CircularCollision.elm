module CircularCollision
    exposing
        ( Circle
        , collision
        , velocityAfterCollision
        , circleAtCollision
        )

import Vector exposing (Vector)


type alias Circle =
    { cx : Float
    , cy : Float
    , radius : Float
    }


collision : Circle -> Circle -> Bool
collision circle1 circle2 =
    let
        dx =
            circle1.cx - circle2.cx

        dy =
            circle1.cy - circle2.cy

        distance =
            Vector.magnitude ( dx, dy )
    in
        distance < circle1.radius + circle2.radius



-- velocityAfterCollision formula based on response from:
-- http://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector


velocityAfterCollision : Circle -> Circle -> Vector -> Vector
velocityAfterCollision stationaryCircle movingCircle velocity =
    let
        mirror =
            unitMirrorVector stationaryCircle movingCircle
    in
        Vector.subtract velocity (Vector.scale (2 * Vector.dotProduct velocity mirror) mirror)


circleAtCollision : Circle -> Circle -> Vector -> Circle
circleAtCollision stationaryCircle movingCircle velocity =
    let
        adjustmentVector =
            adjustmentToCollision stationaryCircle movingCircle velocity

        ( collisionPointX, collisionPointY ) =
            Vector.add ( movingCircle.cx, movingCircle.cy ) adjustmentVector
    in
        Circle collisionPointX collisionPointY movingCircle.radius


adjustmentToCollision : Circle -> Circle -> Vector -> Vector
adjustmentToCollision stationaryCircle movingCircle velocity =
    let
        sumCircleRadii =
            stationaryCircle.radius + movingCircle.radius

        circleCentresDistance =
            Vector.magnitude <|
                vectorBetweenCircleCentres stationaryCircle movingCircle

        negatedVelocityUnitVector =
            velocity
                |> Vector.negate
                |> Vector.normalise
    in
        Vector.scale (2 * (sumCircleRadii - circleCentresDistance)) <|
            negatedVelocityUnitVector


unitMirrorVector : Circle -> Circle -> Vector
unitMirrorVector stationaryCircle movingCircle =
    Vector.normalise <| vectorBetweenCircleCentres stationaryCircle movingCircle


vectorBetweenCircleCentres : Circle -> Circle -> Vector
vectorBetweenCircleCentres circle1 circle2 =
    ( circle2.cx - circle1.cx, circle2.cy - circle1.cy )
