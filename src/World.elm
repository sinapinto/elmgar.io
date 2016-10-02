module World exposing (World, tick, writeVector)

import Collage exposing (..)
import Color exposing (..)
import Text exposing (defaultStyle)
import Player exposing (Player)
import Vector exposing (..)

type alias World =
  { position : Vector }

tick : Float -> Player -> World -> World
tick timeDelta player world =
  { position = world.position <+> (timeDelta *> player.velocity) }

writeVector : Vector -> Form
writeVector vec =
  let
    (x, y) = (round (fst vec), round (snd vec))
    text = "(" ++ toString x ++ ", " ++ toString y ++ ")"
  in
    text
      |> Text.fromString
      |> Text.style { defaultStyle | color = black, bold = True }
      |> Collage.text 
