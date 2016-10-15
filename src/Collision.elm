module Collision exposing (collide)

import List exposing (filter, length)
import Bullets exposing (Bullet)
import Food exposing (Food)
import Player exposing (Player)
import Vector exposing (Vector)
import World exposing (World)

collide : World -> Player -> List Bullet -> List Food -> (Player, List Bullet, List Food)
collide world player bullets foods =
  let
    foods' = filter (not << collidePlayerFood world.position player.radius) foods
    bullets' = filter (not << collidePlayerBullet world.position player.radius) bullets
    player' =
      if length foods' < length foods
      then { player | radius = player.radius + 0.4 }
      else player
    player'' =
      if length bullets' < length bullets
      then { player | radius = player.radius + 2 }
      else player
  in
    (player'', bullets', foods')

collidePlayerFood : Vector -> Float -> Food -> Bool
collidePlayerFood playerPosition playerRadius food =
  let
    (x1, y1) = playerPosition
    (x2, y2) = food.position
    dx = x1 - x2
    dy = y1 - y2
  in
    dx ^ 2 + dy ^ 2 < (playerRadius + food.radius) ^ 2

collidePlayerBullet : Vector -> Float -> Bullet -> Bool
collidePlayerBullet playerPosition playerRadius bullet =
  let
    (x1, y1) = playerPosition
    (x2, y2) = bullet.position
    dx = x1 - x2
    dy = y1 - y2
  in
    dx ^ 2 + dy ^ 2 < (playerRadius + bullet.radius) ^ 2
