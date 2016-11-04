module Circle.Collision
    exposing
        ( nextVelocity
        , anyCollisions
        , partitionCircles
        , velocityAfterCollision
        , applyDamage
        )

import Circle exposing (Circle)
import Vector exposing (Vector)


nextVelocity : Vector -> Circle -> List Circle -> Vector
nextVelocity velocity movingCircle collidingCircles =
    case List.head (collidingCircles) of
        Just collidingCircle ->
            velocityAfterCollision collidingCircle movingCircle velocity

        Nothing ->
            velocity


applyDamage : Circle -> Circle
applyDamage circle =
    { circle | hitPoints = circle.hitPoints - 1 }


partitionCircles : Circle -> List Circle -> ( List Circle, List Circle )
partitionCircles movingCircle stationaryCircles =
    List.partition (collision movingCircle) stationaryCircles


anyCollisions : Circle -> List Circle -> Bool
anyCollisions movingCircle stationaryCircles =
    List.filter (collision movingCircle) stationaryCircles
        |> List.isEmpty
        |> not


collision : Circle -> Circle -> Bool
collision circle1 circle2 =
    let
        distanceBetweenCircleCentres =
            Vector.magnitude <| vectorBetweenCircleCentres circle1 circle2

        tolerance =
            0.1

        collisionDistance =
            circle1.radius + circle2.radius + tolerance
    in
        distanceBetweenCircleCentres <= collisionDistance



-- velocityAfterCollision formula based on response from:
-- http://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector


velocityAfterCollision : Circle -> Circle -> Vector -> Vector
velocityAfterCollision stationaryCircle movingCircle velocity =
    let
        mirror =
            unitMirrorVector stationaryCircle movingCircle
    in
        Vector.subtract velocity (Vector.scale (2 * Vector.dotProduct velocity mirror) mirror)


unitMirrorVector : Circle -> Circle -> Vector
unitMirrorVector stationaryCircle movingCircle =
    Vector.normalise <| vectorBetweenCircleCentres stationaryCircle movingCircle


vectorBetweenCircleCentres : Circle -> Circle -> Vector
vectorBetweenCircleCentres circle1 circle2 =
    ( circle2.cx - circle1.cx, circle2.cy - circle1.cy )
