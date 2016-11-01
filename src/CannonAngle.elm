module CannonAngle exposing (..)


ticksToSvgAngle : Int -> Int
ticksToSvgAngle ticks =
    90 - ticksToAngle ticks


ticksToAngle : Int -> Int
ticksToAngle ticks =
    180 - abs (180 - (ticks % 360))
