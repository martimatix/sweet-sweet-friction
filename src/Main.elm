import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import CircularCollision as CC


main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = Float


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Tick Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      if model > 200 then
        (0, Cmd.none)
      else
        (model + 1, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Tick



-- VIEW


view : Model -> Html Msg
view model =
  svg [ viewBox "0 0 100 500", width "300px" ]
    [ myCircle model
    ]


myCircle : Float -> Svg a
myCircle model =
  let
    y =
      toString (model * -3 + 500)
  in
    circle [ cx "50", cy y, r "45", fill "#0B79CE" ] []
