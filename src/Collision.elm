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
  intersectCircles playerPosition playerRadius food.position food.radius

collidePlayerBullet : Vector -> Float -> Bullet -> Bool
collidePlayerBullet playerPosition playerRadius bullet =
  intersectCircles playerPosition playerRadius bullet.position bullet.radius

intersectCircles : Vector -> Float -> Vector -> Float -> Bool
intersectCircles (x1, y1) r1 (x2, y2) r2 =
    (x1 - x2) ^ 2 + (y1 - y2) ^ 2 < (r1 + r2) ^ 2
