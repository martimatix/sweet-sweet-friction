module Main exposing (..)

import Model exposing (Model)
import Update exposing (update, Msg(..))
import View exposing (view)
import Html exposing (Html)
import AnimationFrame
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model.initial, Task.perform (always Init) (Task.succeed 0) )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
