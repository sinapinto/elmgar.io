module Bullets exposing (Bullet, fire, tick, draw)

import Collage exposing (Form, filled, outlined, circle, move, group, defaultLine)
import Color exposing (Color)
import List exposing (map, filterMap)
import Vector exposing (..)
import Player exposing (Player)
import Ship

type alias Bullet =
  { position : Vector
  , velocity : Vector
  , expire : Float
  }

fire : Player -> Vector -> List Bullet -> List Bullet
fire player world bullets =
  { position = Ship.front world player.rotation
  , velocity = player.velocity
  |> (<+>) (rotate player.rotation (0, 160))
  , expire = 3.0
  } :: bullets


tick : Float -> List Bullet -> List Bullet
tick timeDelta bullets =
  filterMap (moveBullet timeDelta >> killBullet) bullets

moveBullet : Float -> Bullet -> Bullet
moveBullet timeDelta bullet =
  let
    position = bullet.position <+> (timeDelta *> bullet.velocity)
    expire = max 0 (bullet.expire - timeDelta)
  in
    { position = position
    , velocity = bullet.velocity
    , expire = expire
    }

killBullet : Bullet -> Maybe Bullet
killBullet bullet =
  if bullet.expire > 0 then
    Just bullet
  else
    Nothing

draw : (Color, Color) -> List Bullet -> Form
draw colors = group << map (drawBullet colors)

drawBullet : (Color, Color) -> Bullet -> Form
drawBullet (c1, c2) bullet =
  [ circle 5
  |> filled c1
  |> move bullet.position
  , circle 5
  |> outlined { defaultLine | color = c2, width = 3 }
  |> move bullet.position
  ]
    |> group
