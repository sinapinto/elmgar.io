module World exposing (World, tick, draw, writeVector)

import Collage exposing (..)
import Color exposing (..)
import Text exposing (defaultStyle)
import Player exposing (Player)
import Vector exposing (..)

type alias World =
  { position : Vector
  }

tick : Float -> Player -> World -> World
tick timeDelta player world =
  let position = world.position <+> (timeDelta *> player.velocity)
  in
    { position = position
    }

draw : World -> Form
draw world =
  writeVector world.position
    |> move (-500, 200)

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
