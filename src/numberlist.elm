import Array exposing (Array)
import Html.App as Html
import Html exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html.Events exposing (..)
import Time exposing (..)
import Keyboard exposing (KeyCode)
import Bitwise exposing (..)
import Maybe exposing (withDefault)

pong = Array.fromList [106,2,107,12,108,63,109,12,162,234,218,182,220,214,110,0,34,212,102,3,104,2,96,96,240,21,240,7,48,0,18,26,199,23,119,8,105,255,162,240,214,113,162,234,218,182,220,214,96,1,224,161,123,254,96,4,224,161,123,2,96,31,139,2,218,182,96,12,224,161,125,254,96,13,224,161,125,2,96,31,141,2,220,214,162,240,214,113,134,132,135,148,96,63,134,2,97,31,135,18,70,2,18,120,70,63,18,130,71,31,105,255,71,0,105,1,214,113,18,42,104,2,99,1,128,112,128,181,18,138,104,254,99,10,128,112,128,213,63,1,18,162,97,2,128,21,63,1,18,186,128,21,63,1,18,200,128,21,63,1,18,194,96,32,240,24,34,212,142,52,34,212,102,62,51,1,102,3,104,254,51,1,104,2,18,22,121,255,73,254,105,255,18,200,121,1,73,2,105,1,96,4,240,24,118,1,70,64,118,254,18,108,162,242,254,51,242,101,241,41,100,20,101,0,212,85,116,21,242,41,212,85,0,238,128,128,128,128,128,128,128,0,0,0,0,0]

type alias Model =
  { time: Int
  , keys: Array Int
  , memory: Array Int
  , display: Array Int
  , pc: Int
  , stack: Array Int
  , sp: Int

  , regs: Array Int
  , v0: Int
  , v1: Int
  , v2: Int
  , v3: Int
  , v4: Int
  , v5: Int
  , v6: Int
  , v7: Int
  , v8: Int
  , v9: Int
  , va: Int
  , vb: Int
  , vc: Int
  , vd: Int
  , ve: Int
  , vf: Int

  , i: Int
  , dt: Int
  , st: Int
  }

model : Model
model =
  { keys = Array.initialize 16 identity
  , time = 0
  , memory = Array.initialize 4096 (always 0)
  , display = Array.initialize (64*32) (always 0)
  , pc = 0
  , stack = Array.initialize 16 (always 0)
  , sp = 0

  , regs = Array.initialize 16 (always 0)
  , v0 = 0
  , v1 = 0
  , v2 = 0
  , v3 = 0
  , v4 = 0
  , v5 = 0
  , v6 = 0
  , v7 = 0
  , v8 = 0
  , v9 = 0
  , va = 0
  , vb = 0
  , vc = 0
  , vd = 0
  , ve = 0
  , vf = 0

  , i = 0
  , dt = 0
  , st = 0
  }


type Msg
  = Tick Time
  | KeyDown KeyCode
  | NoOp
  | ExecuteInstruction Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      ({ model | time = (round time) }, Cmd.none)
    KeyDown keycode ->
      ({ model | keys = Array.push keycode model.keys }, Cmd.none)
    ExecuteInstruction instruction ->
      let
        opcode = instruction `shiftRight` 12
      in
         case opcode of
            0x0 ->
              ({ model | display = Array.map (\_ -> 0) model.display}, Cmd.none)
            _ ->
              (model, Cmd.none)



    NoOp ->
      (model, Cmd.none)


getWord : Int -> Array Int -> Int
getWord n xs =
  let
    left = xs |> Array.get n |> withDefault 0 |> (flip shiftLeft) 8
    right = xs |> Array.get (n+1) |> withDefault 0
    --left = (withDefault 0 (Array.get n xs)) `shiftLeft` 8
    --right = withDefault 0 (Array.get (n+1) xs)
  in
    left `or` right

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


