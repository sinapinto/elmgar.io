import Html exposing (..)
import Html.App
import Element
import Collage exposing (..)
import Time exposing (Time, inSeconds, inMilliseconds, now)
import Task
import AnimationFrame
import Keyboard
import Window exposing (Size)
import Mouse exposing (Position)
import Bullets exposing (Bullet)
import Player exposing (Player)
import Food exposing (Food)
import World exposing (World)
import Keys exposing (Keys)
import Colors exposing (bg)
import Collision exposing (collide)

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
  , mouse : (Float, Float)
  , window : Size
  , world : World
  }

init : (Model, Cmd Msg)
init =
  { seed = 0
  , player = { position = (0, 0), velocity = (0, 0), rotation = 0, colors = (bg, bg) }
  , bullets = []
  , fireCooldown = 0
  , foods = []
  , keys = { space = False }
  , mouse = (0, 0)
  , window = { width = 0, height = 0 }
  , world = { position = (0, 0) }
  }
  ! [ Task.perform (always NoOp) Init Time.now
    , Task.perform (always NoOp) WindowResize Window.size
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs (inSeconds >> Tick)
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
  | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    Init time ->
      let seed = inMilliseconds time |> floor
      in
        { model
        | seed = seed
        , foods = Food.init seed model.window
        , player = Player.init seed model.player
        }

    Tick timeDelta ->
      tick timeDelta model

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

    NoOp ->
      model

tick : Float -> Model -> Model
tick timeDelta model =
  let
    world = World.tick timeDelta model.player model.world
    foods = Food.tick timeDelta model.foods
    player = Player.tick timeDelta model.mouse model.player
    bullets = Bullets.tick timeDelta model.bullets
    (bullets', fireCooldown) =
      if model.keys.space && model.fireCooldown == 0 then
        (Bullets.fire model.player model.world.position bullets, 0.3)
      else
        (bullets, max 0 (model.fireCooldown - timeDelta))
    (bullets'', foods') = collide bullets' foods
  in
    { model
    | player = player
    , bullets = bullets''
    , fireCooldown = fireCooldown
    , foods = foods'
    , world = world
    }

view : Model -> Html Msg
view model =
  let
    width = model.window.width
    height = model.window.height
    (x, y) = model.world.position
  in
    collage width height
      [ rect (toFloat width) (toFloat height)
      |> filled bg
      , World.draw model.world model.window
      , Food.draw model.foods |> move (-x, -y)
      , Bullets.draw model.player.colors model.bullets |> move (-x, -y)
      , Player.draw model.player model.mouse
      ]
        |> Element.toHtml
