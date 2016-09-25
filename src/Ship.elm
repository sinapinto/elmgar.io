module Ship exposing (draw, front, left, right)

import Collage exposing (..)
import Color exposing (..)
import Vector exposing (..)

reposition : Vector -> Vector -> Float -> Vector
reposition point position rotation =
  position <+> (Vector.rotate rotation point)

front : (Vector -> Float -> Vector)
front = reposition (0, 24)

left : (Vector -> Float -> Vector)
left = reposition (-12, -12)

right : (Vector -> Float -> Vector)
right = reposition (12, -12)

draw : Vector -> Float -> Form
draw position rotation =
  let
    shape =
      polygon
        [ front position rotation
        , left position rotation
        , right position rotation
        ]
  in
    [ shape
    |> filled green
    , shape
    |> outlined { defaultLine | color = white, width = 2 }
    ]
      |> group
