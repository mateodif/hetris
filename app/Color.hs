module Color (Color, skyblue, purple, yellow, blue, orange, green, red, black) where

import Data.Word (Word8)
import Linear

type Color = V4 Word8

skyblue :: Color
skyblue = V4 135 206 235 255

purple :: Color
purple = V4 128 0 128 255

yellow :: Color
yellow = V4 255 255 0 255

blue :: Color
blue = V4 0 0 255 255

orange :: Color
orange = V4 255 165 0 255

green :: Color
green = V4 0 128 0 255

red :: Color
red = V4 255 0 0 255

black :: Color
black = V4 0 0 0 255
