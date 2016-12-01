module Bounds exposing (..)


gameX : Int
gameX =
    500


gameY : Int
gameY =
    600


activeX : Int
activeX =
    gameX


activeY : Int
activeY =
    let
        cannonMargin =
            110
    in
        gameY - cannonMargin
