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
            150
    in
        ( x, y - cannonMargin )
