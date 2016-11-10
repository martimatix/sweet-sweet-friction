module WallCollision exposing (velocityAfterCollision, collision)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds exposing (Bounds)


velocityAfterCollision : Bounds -> Vector -> Circle -> Vector
velocityAfterCollision bounds velocity circle =
    if collisionWithVerticalWall bounds circle then
        velocityAfterVerticalWall velocity
    else if collisionWithHorizontalWall bounds circle then
        velocityAfterHorizontalWall velocity
    else
        velocity


collision : Bounds -> Circle -> Bool
collision bounds circle =
    let
        vertical =
            collisionWithVerticalWall bounds circle

        horizontal =
            collisionWithHorizontalWall bounds circle
    in
        vertical || horizontal


collisionWithVerticalWall : Bounds -> Circle -> Bool
collisionWithVerticalWall bounds { cx, radius } =
    let
        ( boundsX, _ ) =
            bounds
    in
        cx - radius <= 0 || cx + radius >= boundsX


collisionWithHorizontalWall : Bounds -> Circle -> Bool
collisionWithHorizontalWall bounds { cy, radius } =
    let
        ( _, boundsY ) =
            bounds
    in
        cy - radius <= 0 || cy + radius >= boundsY


velocityAfterVerticalWall : Vector -> Vector
velocityAfterVerticalWall ( a, b ) =
    ( -a, b )


velocityAfterHorizontalWall : Vector -> Vector
velocityAfterHorizontalWall ( a, b ) =
    ( a, -b )
