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
    -- position =
    --   player.position <+> (timeDelta *> player.velocity)
    --     |> limitRadius 20

    velocity =
      (0, 50 * timeDelta)
        |> rotate player.rotation
        |> (*>) (distance player.position mouse |> min 200)

    rotation =
      mouse
        |> calcRotation player.position
        |> calcRotationStep timeDelta player.rotation
  in
    { player
    | velocity = velocity
    , rotation = rotation
    }

-- limitRadius : Float -> Vector -> Vector
-- limitRadius limit point =
--   let (r, t) = toPolar point
--   in fromPolar (min r limit, t)

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
