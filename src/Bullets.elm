module Bullets exposing (Bullet, fire, tick, draw)

import Collage exposing (Form, filled, circle, move, group)
import Color exposing (rgb)
import List exposing (map, filterMap)
import Vector exposing (..)
import Player exposing (Player)

type alias Bullet =
  { position : Vector
  , velocity : Vector
  , expire : Float
  }

fire : Player -> List Bullet -> List Bullet
fire player bullets =
  { position = player.position
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

draw : List Bullet -> Form
draw = group << map drawBullet

drawBullet : Bullet -> Form
drawBullet bullet =
  circle 2
    |> filled (rgb 255 255 255)
    |> move bullet.position
