module WallCollision exposing (collision)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds exposing (Bounds)


collision : ( Circle, Vector ) -> ( Circle, Vector )
collision ( activeCircle, velocity ) =
    ( activeCircle, velocity )
        |> verticalCollision
        |> horizontalCollision


verticalCollision : ( Circle, Vector ) -> ( Circle, Vector )
verticalCollision ( { cx, radius } as activeCircle, velocity ) =
    if collisionLeftWall activeCircle then
        let
            nextActiveCircle =
                { activeCircle | cx = 2 * radius - cx }

            nextVelocity =
                velocityAfterVerticalWall velocity
        in
            ( nextActiveCircle, nextVelocity )
    else if collisionRightWall activeCircle then
        let
            boundsX =
                toFloat Bounds.activeX

            nextActiveCircle =
                { activeCircle | cx = 2 * (boundsX - radius) - cx }

            nextVelocity =
                velocityAfterVerticalWall velocity
        in
            ( nextActiveCircle, nextVelocity )
    else
        ( activeCircle, velocity )


horizontalCollision : ( Circle, Vector ) -> ( Circle, Vector )
horizontalCollision ( { cy, radius } as activeCircle, velocity ) =
    if collisionTopWall activeCircle then
        let
            nextActiveCircle =
                { activeCircle | cy = 2 * radius - cy }

            nextVelocity =
                velocityAfterHorizontalWall velocity
        in
            ( nextActiveCircle, nextVelocity )
    else
        ( activeCircle, velocity )


collisionLeftWall : Circle -> Bool
collisionLeftWall { cx, radius } =
    cx - radius <= 0


collisionRightWall : Circle -> Bool
collisionRightWall { cx, radius } =
    cx + radius >= (toFloat Bounds.activeX)


collisionTopWall : Circle -> Bool
collisionTopWall { cy, radius } =
    cy - radius <= 0


velocityAfterVerticalWall : Vector -> Vector
velocityAfterVerticalWall ( a, b ) =
    ( -a, b )


velocityAfterHorizontalWall : Vector -> Vector
velocityAfterHorizontalWall ( a, b ) =
    ( a, -b )
