module Ship exposing (draw)

import Collage exposing (Form, group, polygon, filled)
import Color exposing (..)
import List exposing (map)
import Vector exposing (..)

ship : Vector -> Float -> List Vector
ship position rotation =
  let
    reposition = \point -> position <+> (rotate rotation point)
    points =
      [ (0, 12)
      , (-6, -6)
      , (6, -6)
      ]
  in
    map reposition points

draw : Vector -> Float -> Form
draw position rotation =
  [ ship position rotation
  |> polygon
  |> filled green
  ]
    |> group
