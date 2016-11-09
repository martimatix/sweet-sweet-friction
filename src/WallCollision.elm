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
        ( boundaryX, _ ) =
            bounds
    in
        cx - radius <= 0 || cx + radius >= boundaryX


collisionWithHorizontalWall : Bounds -> Circle -> Bool
collisionWithHorizontalWall bounds { cy, radius } =
    let
        ( _, boundaryY ) =
            bounds
    in
        cy - radius <= 0 || cy + radius >= boundaryY


velocityAfterVerticalWall : Vector -> Vector
velocityAfterVerticalWall ( a, b ) =
    ( -a, b )


velocityAfterHorizontalWall : Vector -> Vector
velocityAfterHorizontalWall ( a, b ) =
    ( a, -b )
