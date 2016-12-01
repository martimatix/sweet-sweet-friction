module Bounds exposing (..)


type alias Bounds =
    ( Int, Int )


game : Bounds
game =
    ( 500, 600 )


active : Bounds
active =
    let
        ( x, y ) =
            game

        cannonMargin =
            110
    in
        ( x, y - cannonMargin )


gameX : Int
gameX =
    Tuple.first game


gameY : Int
gameY =
    Tuple.second game


activeX : Int
activeX =
    Tuple.first active


activeY : Int
activeY =
    Tuple.second active
