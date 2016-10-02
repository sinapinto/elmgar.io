module Food exposing (Food, init, tick, draw)

import Collage exposing (..)
import Colors exposing (..)
import Color exposing (Color)
import Random exposing (..)
import List
import Window exposing (Size)
import Vector exposing (..)

type alias Food =
  { position : Vector
  , color : (Color, Color)
  }

init : Int -> Size -> List Food
init randomInt bounds =
  let
    (left, right) = (toFloat -bounds.width / 2, toFloat bounds.width / 2)
    (bottom, top) = (toFloat -bounds.height / 2, toFloat bounds.height / 2)

    generator =
      map3 (\a b c -> (a, b, c)) (float left right) (float bottom top) (int 0 Colors.max)
        |> list 5
    (randoms, _) = step generator (initialSeed randomInt)
  in
    List.map initFood randoms

initFood : (Float, Float, Int) -> Food
initFood (x, y, c) =
  { position = (x, y)
  , color = getColor c
  }

tick : Float -> List Food -> List Food
tick timeDelta = List.map (tickFood timeDelta)

tickFood : Float -> Food -> Food
tickFood timeDelta food =
  food

draw : List Food -> Form
draw = group << List.map drawFood

drawFood : Food -> Form
drawFood food =
  [ ngon 7 8
  |> filled (fst food.color)
  |> move food.position
  , ngon 7 8
  |> outlined { defaultLine | color = (snd food.color), width = 3 }
  |> move food.position
  ]
    |> group
