module Ship exposing (draw)

import Collage exposing (..)
import Color exposing (..)
import List exposing (map)
import Vector exposing (..)

ship : Vector -> Float -> List Vector
ship position rotation =
  let reposition = \point -> position <+> (Vector.rotate rotation point)
  in
    map reposition
      [ (0, 24)
      , (-12, -12)
      , (12, -12)
      ]

draw : Vector -> Float -> Form
draw position rotation =
  let
    shape = ship position rotation |> polygon
    outline =
      { defaultLine
      | color = white
      , width = 2
      , join = Smooth
      , cap = Round
      }
  in
    [ shape
    |> filled green
    , shape
    |> outlined outline
    ]
      |> group
