module World exposing (World, tick, draw)

import Collage exposing (..)
import List exposing (map)
import Colors exposing (lightGray)
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
    (x, y) = world.position

    gap = 50
    hw = toFloat <| window.width // 2
    hh = toFloat <| window.height // 2
    (left, right) = (-hw, hw)
    (bottom, top) = (-hh, hh)

    drawHorzLine i =
      path [(left, i * gap - hh), (right, i * gap - hh)]
        |> traced (solid lightGray)
    drawVertLine i =
      path [(i * gap - hw, bottom), (i * gap - hw, top)]
        |> traced (solid lightGray)

    numHorzLines = toFloat <| window.height // gap
    numVertLines = toFloat <| window.width // gap
  in
    map drawHorzLine [0..numHorzLines] ++
    map drawVertLine [0..numVertLines]
      |> group
