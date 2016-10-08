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
  , radius : Float
  }

expire : Float
expire = 3

fire : Player -> Vector -> List Bullet -> List Bullet
fire player world bullets =
  { position = Ship.front world player.rotation
  , velocity = player.velocity
  |> (<+>) (rotate player.rotation (0, 160))
  , expire = expire
  , radius = 5
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
    { bullet
    | position = position
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
  let
    bw = 3
    radius = bullet.radius - bw
  in
    [ circle radius
    |> filled c1
    |> move bullet.position
    , circle radius
    |> outlined { defaultLine | color = c2, width = bw }
    |> move bullet.position
    ]
      |> group
