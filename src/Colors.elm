module Colors exposing (max, getColor, bg)

import Color exposing (..)

max : Int
max = 14

getColor : Int -> (Color, Color)
getColor i =
  case i of
    0 -> pink
    1 -> cyan
    2 -> red
    3 -> green
    4 -> teal
    5 -> redOrange
    6 -> lime
    7 -> lightPurple
    8 -> purple
    9 -> gold
    10 -> yellow
    11 -> orange
    12 -> blue
    13 -> darkBlue
    14 -> neonBlue
    _ -> (black, black)

bg : Color
bg = rgb 242 251 255

pink : (Color, Color)
pink = (rgb 232 5 153, rgb 215 6 142)

cyan : (Color, Color)
cyan = (rgb 7 214 255, rgb 6 192 229)

red : (Color, Color)
red = (rgb 244 6 47, rgb 220 5 42)

green : (Color, Color)
green = (rgb 104 255  7, rgb 93 229 6)

teal : (Color, Color)
teal = (rgb 7 197 255, rgb 6 177 229)

redOrange : (Color, Color)
redOrange = (rgb 255 61 7, rgb 229 54 6)

lime : (Color, Color)
lime = (rgb 173 255 7, rgb 155 229 6)

lightPurple : (Color, Color)
lightPurple = (rgb 217 7 255, rgb 195 6 229)

purple : (Color, Color)
purple = (rgb 180 6 22, rgb 180 6 22)

gold : (Color, Color)
gold = (rgb 255 205 7, rgb 229 184 6)

yellow : (Color, Color)
yellow = (rgb 220 255 7, rgb 198 229 6)

orange : (Color, Color)
orange = (rgb 255 78 7, rgb 229 70 6)

blue : (Color, Color)
blue = (rgb 7 81 255, rgb 6 72 229)

darkBlue : (Color, Color)
darkBlue = (rgb 26 7 255, rgb 23 6 229)

neonBlue : (Color, Color)
neonBlue = (rgb 7 243 255, rgb 7 243 255)
