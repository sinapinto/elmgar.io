module Player exposing (draw, Player, tick)

import Collage exposing (Form, defaultLine, segment, traced)
import Color exposing (..)
import Vector exposing (..)
import Ship

type alias Player =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  }

tick : Float -> (Float, Float) -> Player -> Player
tick timeDelta mouse player =
  let
    position = player.position <+> (timeDelta *> player.velocity)
    velocity =
      (0, 50 * timeDelta)
        |> rotate player.rotation
        |> (*>) (calcVelocity (distance player.position mouse))
    rotation =
      mouse
        |> calcRotation player.position
        |> calcRotationStep timeDelta player.rotation
  in
    { player
    | position = position
    , velocity = velocity
    , rotation = rotation
    }

calcVelocity : Float -> Float
calcVelocity distance =
  min 200 (distance * 2)

calcRotation : (Float, Float) -> (Float, Float) -> Float
calcRotation (x, y) (x', y') =
  atan2 (x' - x) (y' - y)

calcRotationStep : Float -> Float -> Float -> Float
calcRotationStep timeDelta old new =
  let
    delta = new - old
    delta' =
      if delta > pi then delta - 2 * pi
      else if delta < -pi then delta + 2 * pi
      else delta
    result = old + (delta' * 10 * timeDelta)
  in
    if result > 2 * pi then result - 2 * pi
    else if result < 0 then result + 2 * pi
    else result

draw : Player -> (Float, Float) -> Form
draw player mouse =
  [ Ship.draw player.position player.rotation
  , segment player.position mouse
  |> traced { defaultLine | color = red }
  ]
    |> Collage.group
