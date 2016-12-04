module Main exposing (..)

import Model exposing (Model)
import Update exposing (update, Msg(..))
import View exposing (view)
import Html exposing (Html)
import AnimationFrame
import Task
import Window
import Task


main : Program Never Model Msg
main =
    Html.program
        { init =
            Model.initial
                ! [ Task.perform (always Init) (Task.succeed 0)
                  , initialSizeCmd
                  ]
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes sizeToMsg
        ]



-- WINDOW RESIZE


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg Window.size


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize size
