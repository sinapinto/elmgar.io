module Player exposing (draw, Player, tick)

import Collage exposing (Form)
import Mouse exposing (Position)
import Window exposing (Size)
import Vector exposing (..)
import Ship

type alias Player =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

tick : Float -> Position -> Size -> Player -> Player
tick timeDelta mouse window player =
  let
    position = player.position <+> (timeDelta *> player.velocity)
    velocity =
      (0, 50 * timeDelta)
      |> rotate player.rotation
    mouse' =
      ( mouse.x - window.width  // 2 |> toFloat
      , mouse.y - window.height // 2 |> toFloat
      )
    rotation = calcRotation mouse' player.position
    rotation' = abs (2 * pi - rotation) -- clockwise
    rotationDelta = rotation' - player.rotation
    rotation'' = player.rotation + rotationDelta * timeDelta * 10
  in
    { player
    | position = position
    , velocity = velocity
    , rotation = rotation''
    }

calcRotation : (Float, Float) -> (Float, Float) -> Float
calcRotation (x, y) (x', y') =
  atan2 (x - x') (y - y') + pi

draw : Player -> Form
draw player =
  Ship.draw player.position player.rotation
