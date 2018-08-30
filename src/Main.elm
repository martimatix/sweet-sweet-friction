module Main exposing (initialSizeCmd, main, sizeToMsg, subscriptions)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Html exposing (Html)
import Model exposing (Model)
import Task
import Update exposing (Msg(..), update)
import View exposing (view)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Int -> ( Model, Cmd Msg )
init highScore =
    ( Model.initial highScore
    , Cmd.batch
        [ Task.perform (always Init) (Task.succeed 0)
        , initialSizeCmd
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onAnimationFrameDelta Tick
        , Events.onResize sizeToMsg
        ]



-- WINDOW RESIZE


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform initViewport Dom.getViewport


initViewport : Dom.Viewport -> Msg
initViewport { viewport } =
    WindowResize (round viewport.width) (round viewport.height)


sizeToMsg : Int -> Int -> Msg
sizeToMsg x y =
    WindowResize x y
