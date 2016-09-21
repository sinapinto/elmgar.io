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
    angle = abs <| 2 * pi - (calcAngle mouse' player.position)
    rotation = calcRotation player.rotation angle timeDelta
  in
    { player
    | position = position
    , velocity = velocity
    , rotation = rotation
    }

calcAngle : (Float, Float) -> (Float, Float) -> Float
calcAngle (x, y) (x', y') =
  atan2 (x - x') (y - y') + pi

calcRotation : Float -> Float -> Float -> Float
calcRotation old new timeDelta =
  let
    delta = new - old
    delta' =
      if abs delta > pi
      then delta + 2 * pi
      else delta
    result = old + (delta' * 30 * timeDelta)
  in
    if result > 2 * pi
    then result - 2 * pi
    else result


draw : Player -> Form
draw player =
  Ship.draw player.position player.rotation
