module Food exposing (Food, init, tick, draw)

import Collage exposing (..)
import Color exposing (red)
import Random exposing (step, float, map2, list, initialSeed)
import List exposing (map)
import Window exposing (Size)
import Vector exposing (..)

type alias Food =
  { position : Vector
  }

init : Int -> Size -> List Food
init randomInt bounds =
  let
    (left, right) = (toFloat -bounds.width/2, toFloat bounds.width/2)
    (bottom, top) = (toFloat -bounds.height/2, toFloat bounds.height/2)
    generator = map2 (,) (float left right) (float bottom top) |> list 10
    (randoms, seed) = step generator (initialSeed randomInt)
  in
    map initFood randoms

initFood : (Float, Float) -> Food
initFood position =
  { position = position
  }

tick : Float -> List Food -> List Food
tick timeDelta = map (tickFood timeDelta)

tickFood : Float -> Food -> Food
tickFood timeDelta food =
  food

draw : List Food -> Form
draw = group << map drawFood

drawFood : Food -> Form
drawFood food =
    circle 4
      |> filled red
      |> move food.position
