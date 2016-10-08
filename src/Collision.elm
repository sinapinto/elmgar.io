module Collision exposing (collide)

import List exposing (filter, any)
import Bullets exposing (Bullet)
import Food exposing (Food)
import Player exposing (Player)
import World exposing (World)

collide : World -> Player -> List Bullet -> List Food -> (List Bullet, List Food)
collide world player bullets foods =
  let foods' = filter (not << collidePlayerFood world player) foods
  in (bullets, foods')

collidePlayerFood : World -> Player -> Food -> Bool
collidePlayerFood world player food =
  let
    (x1, y1) = world.position
    (x2, y2) = food.position
    dx = x1 - x2
    dy = y1 - y2
  in
    dx ^ 2 + dy ^ 2 < (player.radius + food.radius) ^ 2
