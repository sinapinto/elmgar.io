module Food exposing (Food, init, draw, radius)

import Collage exposing (..)
import Colors exposing (..)
import Color exposing (Color)
import Random exposing (..)
import List
import Window exposing (Size)
import Vector exposing (..)

type alias Food =
  { position : Vector
  , color : Color
  }

radius : Float
radius = 10

init : Int -> Size -> List Food
init randomInt bounds =
  let
    (left, right) = (toFloat -bounds.width * 1.5, toFloat bounds.width * 1.5)
    (bottom, top) = (toFloat -bounds.height * 1.5, toFloat bounds.height * 1.5)

    generator =
      map3
        (\a b c -> (a, b, c))
        (float left right)
        (float bottom top)
        (int 0 Colors.max)
        |> list 300
    (randoms, _) = step generator (initialSeed randomInt)
  in
    List.map initFood randoms

initFood : (Float, Float, Int) -> Food
initFood (x, y, colorIndex) =
  { position = (x, y)
  , color = fst (getColor colorIndex)
  }

draw : List Food -> Form
draw =
  let shape = ngon 7 radius
  in group << List.map (drawFood shape)

drawFood : Shape -> Food -> Form
drawFood shape food =
  shape
    |> filled food.color
    |> move food.position
