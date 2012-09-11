module Lib.Css.Helper
  (
  -- Meta description
    textEmphColor
  , textColor
  , textSecondaryColor
  , backHighlightColor
  , background
  , foreground
  , altbackground
  , altforeground
  , alttextEmphColor

  -- specific (mixins)
  , headerbackground
  , footerbackground

  -- colors
  , yellow
  , orange
  , red
  , magenta
  , violet
  , blue
  , cyan
  , green

  -- functions
  , cssGradient
  , cssVerticalGradient
  , light
  , shadow

  -- bases
  , base03
  , base02
  , base01
  , base00
  , base0
  , base1
  , base2
  , base3

  -- variant colors
  , lightyellow
  , lightorange
  , lightred
  , lightcyan
  , shadoworange
  , shadowred
  , shadowyellow
  , white
  , black
  , fullwhite
  , fullblack
  , slightlywhite
  , slightlyblack

  )
where

import Prelude
import Numeric (readHex)
import Data.List (intercalate)


headerbackground, footerbackground :: String

headerbackground=cssVerticalGradient "100%" base02 altbackground
footerbackground=cssVerticalGradient "3em" base02 altbackground

background, backHighlightColor, foreground :: String
textColor, textSecondaryColor, textEmphColor :: String

background=base3
backHighlightColor=base2
foreground=base00
textColor=foreground
textSecondaryColor=base1
textEmphColor=base01

altbackground, altbackHighlightColor, altforeground, alttextcolor :: String
alttextSecondaryColor, alttextEmphColor :: String

altbackground=base01
altbackHighlightColor=base02
altforeground=base0
alttextcolor=altforeground
alttextSecondaryColor=base01
alttextEmphColor=base1

base03, base02, base01, base00, base0, base1, base2, base3 :: String

base03="#002b36"
base02="#073642"
base01="#586e75"
base00="#657b83"
base0="#839496"
base1="#93a1a1"
base2="#eee8d5"
base3="#fdf6e3"

yellow , orange , red , magenta , violet , blue , cyan , green :: String

yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

light :: String -> String
light color = "rgba("++(cssToRgb color)++",0.1)"

lightred , lightorange , lightcyan , lightyellow :: String

lightred    = light red
lightorange = light orange
lightcyan   = light cyan
lightyellow = light yellow

shadow :: String -> String
shadow color = "rgba("++(cssToRgb color)++",0.5)"

shadoworange, shadowred, shadowyellow :: String

shadoworange  = shadow orange
shadowred     = shadow red
shadowyellow  = shadow yellow

-- Transparent colors

fullwhite, white, slightlywhite :: String

fullwhite     = "rgba(255,255,255,1)"
white         = "rgba(255,255,255,0.6)"
slightlywhite = "rgba(255,255,255,0.1)"

fullblack, black, slightlyblack :: String

fullblack     = "rgba(0,43,54,1)"
black         = "rgba(0,43,54,0.6)"
slightlyblack = "rgba(0,43,54,0.1)"

-- Hexa style colors
scalarFromHex :: String ->  Int
scalarFromHex = fst . head . readHex

-- | Color from CSS style color string
cssToRgb :: String -> String
cssToRgb ('#':rd:ru:gd:gu:bd:bu:[]) = intercalate "," $
  map (show . scalarFromHex) [[rd,ru],[gd,gu],[bd,bu]]
cssToRgb ('#':r:g:b:[]) = cssToRgb ['#',r,r,g,g,b,b]
cssToRgb _ = error "Bad color!!!!"

cssGradient :: String -> String -> String
cssGradient from to =
  "background: rgb("++rgbfrom++");" ++
  "background: -moz-radial-gradient(center, ellipse cover,  rgba("++rgbfrom++",1) 0%, rgba("++rgbto++",1) 100%); /* FF3.6+ */" ++
  "background: -webkit-gradient(radial, center center, 0px, center center, 100%, color-stop(0%,rgba("++rgbfrom++",1)), color-stop(100%,rgba("++rgbto++",1))); /* Chrome,Safari4+ */" ++
  "background: -webkit-radial-gradient(center, ellipse cover,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* Chrome10+,Safari5.1+ */" ++
  "background: -o-radial-gradient(center, ellipse cover,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* Opera 12+ */" ++
  "background: -ms-radial-gradient(center, ellipse cover,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* IE10+ */" ++
  "background: radial-gradient(ellipse at center,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* W3C */" ++
  "filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='"++from++"', endColorstr='"++to++"',GradientType=1 ); /* IE6-9 fallback on horizontal gradient */"
  where
      rgbfrom = cssToRgb from
      rgbto   = cssToRgb to

cssVerticalGradient :: String -> String -> String -> String
cssVerticalGradient height from to =
    "background: rgb("++rgbfrom++"); /* Old browsers */" ++
    "background: -moz-linear-gradient(top,  rgba("++rgbfrom++",1) 0%, rgba("++rgbto++",1) "++height++"); /* FF3.6+ */" ++
    "background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,rgba("++rgbfrom++",1)), color-stop("++height++",rgba("++rgbto++",1))); /* Chrome,Safari4+ */" ++
    "background: -webkit-linear-gradient(top,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) "++height++"); /* Chrome10+,Safari5.1+ */" ++
    "background: -o-linear-gradient(top,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) "++height++"); /* Opera 11.10+ */" ++
    "background: -ms-linear-gradient(top,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) "++height++"); /* IE10+ */" ++
    "background: linear-gradient(to bottom,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) "++height++"); /* W3C */" ++
    "filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='"++from++"', endColorstr='"++to++"',GradientType=0 ); /* IE6-9 */"
  where
      rgbfrom = cssToRgb from
      rgbto   = cssToRgb to
