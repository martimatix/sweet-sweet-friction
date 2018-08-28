module Circle.Collision exposing
    ( anyCollisions
    , applyDamage
    , nextVelocity
    , partitionCircles
    , velocityAfterCollision
    )

import Circle exposing (Circle)
import Vector exposing (Vector)


nextVelocity : Vector -> Circle -> List Circle -> Vector
nextVelocity velocity activeCircle collidingCircles =
    case List.head collidingCircles of
        Just collidingCircle ->
            velocityAfterCollision collidingCircle activeCircle velocity

        Nothing ->
            velocity


applyDamage : Circle -> Circle
applyDamage circle =
    { circle | hitPoints = circle.hitPoints - 1 }


partitionCircles : Circle -> List Circle -> ( List Circle, List Circle )
partitionCircles activeCircle stationaryCircles =
    List.partition (collision activeCircle) stationaryCircles


anyCollisions : Circle -> List Circle -> Bool
anyCollisions activeCircle stationaryCircles =
    List.filter (collision activeCircle) stationaryCircles
        |> List.isEmpty
        |> not


collision : Circle -> Circle -> Bool
collision circle1 circle2 =
    let
        distanceBetweenCircleCentres =
            Vector.magnitude <| Circle.vectorBetweenCentres circle1 circle2

        tolerance =
            0.1

        collisionDistance =
            circle1.radius + circle2.radius + tolerance
    in
    distanceBetweenCircleCentres <= collisionDistance



-- velocityAfterCollision formula based on response from:
-- http://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector


velocityAfterCollision : Circle -> Circle -> Vector -> Vector
velocityAfterCollision stationaryCircle activeCircle velocity =
    let
        mirror =
            unitMirrorVector stationaryCircle activeCircle
    in
    Vector.subtract velocity (Vector.scale (2 * Vector.dotProduct velocity mirror) mirror)


unitMirrorVector : Circle -> Circle -> Vector
unitMirrorVector stationaryCircle activeCircle =
    Vector.normalise <| Circle.vectorBetweenCentres stationaryCircle activeCircle
