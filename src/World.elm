module World exposing (World, tick, draw)

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
  let (x, y) = (round (fst world.position), round (snd world.position))
  in
    [ write ("(" ++ toString x ++ ", " ++ toString y ++ ")")
    |> move (-500, 200)
    ]
      |> group

write : String -> Form
write text =
  text
    |> Text.fromString
    |> Text.style { defaultStyle | color = white, bold = True }
    |> Collage.text 
