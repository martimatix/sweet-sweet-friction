module Circle.Collision exposing (nextVelocity, collisionCircle)

import Circle exposing (Circle)
import Vector exposing (Vector)


nextVelocity : Vector -> Circle -> List Circle -> Vector
nextVelocity velocity movingCircle stationaryCircles =
    case collisionCircle movingCircle stationaryCircles of
        Just stationaryCircle ->
            velocityAfterCollision stationaryCircle movingCircle velocity

        Nothing ->
            velocity


collisionCircle : Circle -> List Circle -> Maybe Circle
collisionCircle movingCircle stationaryCircles =
    List.filter (collision movingCircle) stationaryCircles
        |> List.head


collision : Circle -> Circle -> Bool
collision circle1 circle2 =
    let
        distanceBetweenCircleCentres =
            Vector.magnitude <| vectorBetweenCircleCentres circle1 circle2
    in
        distanceBetweenCircleCentres < circle1.radius + circle2.radius



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
