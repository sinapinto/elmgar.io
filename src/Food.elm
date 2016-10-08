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
  , radius : Float
  }

init : Int -> Size -> List Food
init randomInt bounds =
  let
    (left, right) = (toFloat -bounds.width / 2, toFloat bounds.width / 2)
    (bottom, top) = (toFloat -bounds.height / 2, toFloat bounds.height / 2)

    generator =
      map3
        (\a b c -> (a, b, c))
        (float left right)
        (float bottom top)
        (int 0 Colors.max)
        |> list 20
    (randoms, _) = step generator (initialSeed randomInt)
  in
    List.map initFood randoms

initFood : (Float, Float, Int) -> Food
initFood (x, y, colorIndex) =
  { position = (x, y)
  , color = getColor colorIndex
  , radius = 8
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
  let
    bw = 3
    radius = food.radius - bw
  in
    [ circle radius
    |> filled (fst food.color)
    |> move food.position
    , circle radius
    |> outlined { defaultLine | color = (snd food.color), width = bw }
    |> move food.position
    ]
      |> group
