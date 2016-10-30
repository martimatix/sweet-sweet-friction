module CannonAngle exposing (..)


ticksToAngle : Int -> Int
ticksToAngle ticks =
    90 - abs (180 - (ticks % 360))
