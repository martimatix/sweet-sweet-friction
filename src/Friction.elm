module Friction exposing (Result(..), apply)

import Vector exposing (Vector)


type Result
    = SlowsDownCircle Vector
    | CausesStop


frictionMagnitude : Float
frictionMagnitude =
    0.007


apply : Vector -> Result
apply velocity =
    if Vector.magnitude velocity > frictionMagnitude then
        SlowsDownCircle (Vector.add velocity (frictionVector velocity))
    else
        CausesStop


frictionVector : Vector -> Vector
frictionVector velocity =
    velocity
        |> Vector.normalise
        |> Vector.negate
        |> Vector.scale frictionMagnitude
