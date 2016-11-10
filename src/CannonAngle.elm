module CannonAngle exposing (..)


ticksToSvgAngle : Int -> Float
ticksToSvgAngle ticks =
    toFloat (90 - ticksToAngle ticks)


ticksToAngle : Int -> Int
ticksToAngle ticks =
    180 - abs (180 - (ticks % 360))
