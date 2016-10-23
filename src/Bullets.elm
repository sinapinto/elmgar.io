module Bullets exposing (Bullet, fire, tick, draw, radius)

import Collage exposing (Form, filled, outlined, circle, move, group, defaultLine)
import Color exposing (Color)
import List exposing (map, map)
import Vector exposing (..)
import Player exposing (Player)

type alias Bullet =
  { position : Vector
  , velocity : Vector
  }

radius : Float
radius = 16

fire : Player -> Vector -> List Bullet -> List Bullet
fire player world bullets =
  { position = Player.front world player.rotation player.radius
  , velocity = player.velocity <+> rotate player.rotation (0, 500)
  } :: bullets


tick : Float -> List Bullet -> List Bullet
tick timeDelta bullets =
  map (moveBullet timeDelta) bullets

moveBullet : Float -> Bullet -> Bullet
moveBullet timeDelta bullet =
  let
    position = bullet.position <+> (timeDelta *> bullet.velocity)
    velocity =
      if abs (fst bullet.velocity) < 10
      then (0, 0)
      else 0.98 *> bullet.velocity
  in
    { bullet
    | position = position
    , velocity = velocity
    }

draw : (Color, Color) -> List Bullet -> Form
draw colors = group << map (drawBullet colors)

drawBullet : (Color, Color) -> Bullet -> Form
drawBullet (c1, c2) bullet =
  [ circle radius
  |> filled c1
  |> move bullet.position
  , circle radius
  |> outlined { defaultLine | color = c2, width = 5 }
  |> move bullet.position
  ]
    |> group
