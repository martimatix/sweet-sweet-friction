module Main exposing (..)

import Model exposing (Model)
import Update exposing (update, Msg(..))
import View exposing (view)
import Html.App as App
import AnimationFrame


main : Program Never
main =
    App.program
        { init = ( Model.initial, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
