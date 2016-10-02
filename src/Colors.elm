module Colors exposing (..)

import Color exposing (..)

bg : Color
bg = rgb 242 251 255
-- bg = rgb 9 9 9

pink : Color
pink = rgb 232 5 153

darkPink : Color
darkPink = rgb 215 6 142

cyan : Color
cyan = rgb 7 214 255

darkCyan : Color
darkCyan = rgb 6 192 229

red : Color
red = rgb 244 6 47

darkRed : Color
darkRed = rgb 220 5 42

green : Color
green = rgb 104 255  7

darkGreen : Color
darkGreen = rgb 93 229 6

teal : Color
teal = rgb 7 197 255

darkTeal : Color
darkTeal = rgb 6 177 229

redOrange : Color
redOrange = rgb 255 61 7

darkRedOrange : Color
darkRedOrange = rgb 229 54 6

lime : Color
lime = rgb 173 255 7

darkLime : Color
darkLime = rgb 155 229 6

purple : Color
purple = rgb 180 6 22

darkPurple : Color
darkPurple = rgb 84 6 234

getColor : Int -> (Color, Color)
getColor i =
  case i of
    0 -> (pink, darkPink)
    1 -> (cyan, darkCyan)
    2 -> (red, darkRed)
    3 -> (green, darkGreen)
    4 -> (teal, darkTeal)
    5 -> (redOrange, darkRedOrange)
    6 -> (lime, darkLime)
    7 -> (purple, darkPurple)
    _ -> (purple, darkPurple)

max : Int
max = 7
