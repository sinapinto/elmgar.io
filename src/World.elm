module World exposing (World, tick, draw)

import Collage exposing (..)
import List exposing (map)
import Colors exposing (bg, lightGray)
import Player exposing (Player)
import Vector exposing (..)
import Window exposing (Size)

type alias World =
  { position : Vector }

tick : Float -> Player -> World -> World
tick timeDelta player world =
  { position = world.position <+> (timeDelta *> player.velocity) }

draw : World -> Size -> Form
draw world window =
  let
    gap = 38
    (width, height) = (window.width, window.height)
    (x, y) = world.position

    halfWidth = width // 2 |> toFloat
    halfHeight = height // 2 |> toFloat

    (left, right) = (-halfWidth, halfWidth)
    (bottom, top) = (-halfHeight, halfHeight)

    horzOffset = (floor y) % gap
    vertOffset = (floor x) % gap

    drawHorzLine i = path [(left, i), (right, i)] |> traced (solid lightGray)
    drawVertLine i = path [(i, bottom), (i, top)] |> traced (solid lightGray)

    horzIndices = map (toFloat << \i -> i * gap - (height // 2) - horzOffset) [0..(height // gap + 1)]
    vertIndices = map (toFloat << \i -> i * gap - (width // 2)  - vertOffset) [0..(width  // gap + 1)]
  in
    (rect (toFloat width) (toFloat height) |> filled bg) ::
    map drawHorzLine horzIndices ++
    map drawVertLine vertIndices
      |> group
