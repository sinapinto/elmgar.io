module Ship exposing (draw, front, left, right)

import Collage exposing (..)
import Vector exposing (..)
import Color exposing (Color)

reposition : Vector -> Vector -> Float -> Vector
reposition point position rotation =
  position <+> (Vector.rotate rotation point)

front : Vector -> Float -> Vector
front = reposition (0, 22)

left : Vector -> Float -> Vector
left = reposition (-16, -16)

right : Vector -> Float -> Vector
right = reposition (16, -16)

draw : Vector -> Float -> (Color, Color) -> Form
draw position rotation colors =
  let
    shape =
      polygon
        [ front position rotation
        , left position rotation
        , right position rotation
        ]
    outline =
      { defaultLine
      | color = snd colors
      , width = 4
      , cap = Round
      , join = Smooth
      }
  in
    [ shape
    |> filled (fst colors)
    , shape
    |> outlined outline
    ]
      |> group
