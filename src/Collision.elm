module Collision exposing (collide)

import List exposing (filter, any)
import Bullets exposing (Bullet)
import Food exposing (Food)

collide : List Bullet -> List Food -> (List Bullet, List Food)
collide bullets foods =
  if bullets == [] || foods == [] then
    (bullets, foods)
  else
    let
      bullets' = filter (\bullet -> not <| any (intersectBulletFood bullet) foods) bullets
      foods' = filter (\food -> not <| any (flip intersectBulletFood food) bullets) foods
    in
      (bullets', foods')

intersectBulletFood : Bullet -> Food -> Bool
intersectBulletFood bullet food =
  let
    (x1, y1) = bullet.position
    (x2, y2) = food.position
    dx = x1 - x2
    dy = y1 - y2
  in
    dx ^ 2 + dy ^ 2 < (bullet.radius + food.radius) ^ 2
