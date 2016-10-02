module Collision exposing (collide)

import Bullets exposing (Bullet)
import Food exposing (Food)

collide : List Bullet -> List Food -> (List Bullet, List Food)
collide bullets foods =
  (bullets, foods)
