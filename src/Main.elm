import Html exposing (..)
import Html.App
import Element
import Collage exposing (..)
import Time exposing (Time, inSeconds, inMilliseconds, now)
import Task
import AnimationFrame
import Color exposing (..)
import Keyboard
import Window exposing (Size)
import Mouse exposing (Position)
import Bullets exposing (Bullet)
import Player exposing (Player)
import Food exposing (Food)
import Keys exposing (Keys)

main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = \msg model -> update msg model ! []
    , subscriptions = subscriptions
    }

type alias Model =
  { seed : Int
  , player : Player
  , bullets : List Bullet
  , fireCooldown : Float
  , foods : List Food
  , keys : Keys
  , window : Size
  , mouse : (Float, Float)
  }

init : (Model, Cmd Msg)
init =
  (,)
  { seed = 0
  , player =
    { position = (0, 0)
    , velocity = (0, 0)
    , rotation = 0
    }
  , bullets = []
  , fireCooldown = 0
  , foods = []
  , keys = { space = False }
  , window = { width = 0, height = 0 }
  , mouse = (0, 0)
  }
  (Cmd.batch
    [ Task.perform NoOp Init Time.now
    , Task.perform NoOp WindowResize Window.size
    ])

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs Tick
    , Keyboard.downs Key
    , Keyboard.ups (negate >> Key)
    , Window.resizes WindowResize
    , Mouse.moves MouseMove
    ]

type Msg
  = Init Time
  | Tick Time
  | Key Int
  | WindowResize Size
  | MouseMove Position
  | NoOp Time

update : Msg -> Model -> Model
update msg model =
  case msg of
    Init time ->
      let seed = inMilliseconds time |> floor
      in
        { model
        | seed = seed
        , foods = Food.init seed model.window
        }

    Tick timeDelta ->
      tick (inSeconds timeDelta) model

    Key keyCode ->
      { model | keys = Keys.update keyCode model.keys }

    WindowResize size ->
      { model | window = size }

    MouseMove { x, y } ->
      let
        (width, height) = (model.window.width, model.window.height)
        mouse =
          ( toFloat <| x - width // 2
          , toFloat <| (height - y) - height // 2
          )
      in
        { model | mouse = mouse }

    NoOp _ ->
      model

tick : Float -> Model -> Model
tick timeDelta model =
  let
    cd = 0.1
    foods = Food.tick timeDelta model.foods
    player = Player.tick timeDelta model.mouse model.player
    bullets = Bullets.tick timeDelta model.bullets
    (bullets', fireCooldown) =
      if model.keys.space && model.fireCooldown == 0 then
        (Bullets.fire model.player bullets, cd)
      else
        (bullets, max 0 (model.fireCooldown - timeDelta))
  in
    { model
    | player = player
    , bullets = bullets'
    , fireCooldown = fireCooldown
    , foods = foods
    }

view : Model -> Html Msg
view model =
  let
    width = model.window.width
    height = model.window.height
  in
    collage width height
      [ rect (toFloat width) (toFloat height)
      |> filled black
      , Food.draw model.foods
      , Player.draw model.player model.mouse
      , Bullets.draw model.bullets
      ]
        |> Element.toHtml
