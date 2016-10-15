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
    horzOffset = (floor y) % gap |> toFloat
    vertOffset = (floor x) % gap |> toFloat

    halfWidth = width // 2 |> toFloat
    halfHeight = height // 2 |> toFloat
    (left, right) = (-halfWidth, halfWidth)
    (bottom, top) = (-halfHeight, halfHeight)

    drawHorzLine index =
      let i = toFloat index
      in
        path
          [ (left, i * gap - halfHeight - horzOffset)
          , (right, i * gap - halfHeight - horzOffset)
          ]
            |> traced (solid lightGray)

    drawVertLine index =
      let i = toFloat index
      in
        path
          [ (i * gap - halfWidth - vertOffset, bottom)
          , (i * gap - halfWidth - vertOffset, top)
          ]
            |> traced (solid lightGray)
  in
    (rect (toFloat width) (toFloat height) |> filled bg) ::
    map drawHorzLine [0..(height // gap + 1)] ++
    map drawVertLine [0..(width // gap + 1)]
      |> group
