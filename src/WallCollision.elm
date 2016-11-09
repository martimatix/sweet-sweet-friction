module WallCollision exposing (velocityAfterCollision, collision)

import Circle exposing (Circle)
import Vector exposing (Vector)
import Bounds exposing (Bounds)


velocityAfterCollision : Vector -> Circle -> Vector
velocityAfterCollision velocity circle =
    if collisionWithVerticalWall circle then
        velocityAfterVerticalWall velocity
    else if collisionWithHorizontalWall circle then
        velocityAfterHorizontalWall velocity
    else
        velocity


collision : Circle -> Bool
collision circle =
    let
        vertical =
            collisionWithVerticalWall circle

        horizontal =
            collisionWithHorizontalWall circle
    in
        vertical || horizontal


collisionWithVerticalWall : Circle -> Bool
collisionWithVerticalWall { cx, radius } =
    let
        ( boundaryX, _ ) =
            Bounds.game
    in
        cx - radius <= 0 || cx + radius >= boundaryX


collisionWithHorizontalWall : Circle -> Bool
collisionWithHorizontalWall { cy, radius } =
    let
        ( _, boundaryY ) =
            Bounds.game
    in
        cy - radius <= 0 || cy + radius >= boundaryY


velocityAfterVerticalWall : Vector -> Vector
velocityAfterVerticalWall ( a, b ) =
    ( -a, b )


velocityAfterHorizontalWall : Vector -> Vector
velocityAfterHorizontalWall ( a, b ) =
    ( a, -b )
