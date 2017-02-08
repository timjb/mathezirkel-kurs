module NiceColours
  ( navy, blue, aqua, teal, olive, green, lime, yellow
  , orange, red, maroon, fuchsia
  ) where

--import Data.Colour
import Data.Colour.SRGB


-- Nice colours from http://clrs.cc/
blue, teal, olive, green, yellow, orange, red, maroon, fuchsia :: (Ord a, Floating a) => Colour a
navy = sRGB24 0 31 63
blue = sRGB24 0 116 217
aqua = sRGB24 127 219 255
teal = sRGB24 57 204 204
olive = sRGB24 61 153 112
green = sRGB24 46 204 64
lime = sRGB24 1 255 112
yellow = sRGB24 255 220 0
orange = sRGB24 255 133 27
red = sRGB24 255 65 54
maroon = sRGB24 133 20 75
fuchsia = sRGB24 240 18 190