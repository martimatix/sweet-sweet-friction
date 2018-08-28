module Bounds exposing (activeX, activeY, gameX, gameY)


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
