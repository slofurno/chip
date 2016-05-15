import Array exposing (Array)
import Html.App as Html
import Html exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Events exposing (..)
import Time exposing (..)
import Keyboard exposing (KeyCode)

type alias Model =
  { time: Int
  , keys: Array Int
  }


type Msg
  = Tick Time
  | KeyDown KeyCode
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      ({ model | time = (round time) }, Cmd.none)
    KeyDown keycode ->
      ({ model | keys = Array.push keycode model.keys }, Cmd.none)
    NoOp ->
      (model, Cmd.none)

model : Model
model =
  { keys = Array.initialize 4 identity
  , time = 0
  }

subscriptions model =
  let
    ticks =
      every ((1000/60) * millisecond) Tick
    keydown =
      Keyboard.downs KeyDown
  in
    [ticks, keydown] |> Sub.batch

init : (Model, Cmd Msg)
init =
  (model, Cmd.none)

main =
  Html.program { init = init, subscriptions = subscriptions, view = view, update = update }

view : Model -> Html Msg
view {time, keys} =
  let
    items =
      Array.map numberSpan keys
  in
    section [] [
      span [] [text (toString time)],
      div [] (Array.toList items)
    ]

numberSpan : Int -> Html Msg
numberSpan n =
  span [] [text ((toString n) ++ ", ")]
