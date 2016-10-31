module Friction exposing (apply)

import Vector exposing (Vector)


frictionMagnitude : Float
frictionMagnitude =
    0.007


apply : Vector -> Vector
apply velocity =
    if Vector.magnitude velocity > frictionMagnitude then
        Vector.add velocity (frictionVector velocity)
    else
        ( 0, 0 )


frictionVector : Vector -> Vector
frictionVector velocity =
    velocity
        |> Vector.normalise
        |> Vector.negate
        |> Vector.scale frictionMagnitude
