module WallCollision exposing (..)

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


collisionWithVerticalWall : Bounds -> Circle -> Bool
collisionWithVerticalWall ( boundaryX, _ ) { cx, radius } =
    cx - radius < 0 || cx + radius > boundaryX


collisionWithHorizontalWall : Bounds -> Circle -> Bool
collisionWithHorizontalWall ( _, boundaryY ) { cy, radius } =
    cy - radius < 0 || cy + radius > boundaryY


velocityAfterVerticalWall : Vector -> Vector
velocityAfterVerticalWall ( a, b ) =
    ( -a, b )


velocityAfterHorizontalWall : Vector -> Vector
velocityAfterHorizontalWall ( a, b ) =
    ( a, -b )
