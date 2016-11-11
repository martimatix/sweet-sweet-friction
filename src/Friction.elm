module Friction exposing (Result(..), apply)

import Vector exposing (Vector)


type Result
    = SlowsDownCircle Vector
    | CausesStop


frictionCoefficient : Float
frictionCoefficient =
    0.9895


apply : Vector -> Result
apply velocity =
    if Vector.magnitude velocity > 0.08 then
        SlowsDownCircle (Vector.scale frictionCoefficient velocity)
    else
        CausesStop
