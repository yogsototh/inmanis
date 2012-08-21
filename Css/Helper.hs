module Css.Helper
  ( cssGradient
  , cssVerticalGradient
  , background
  , foreground
  , altbackground
  , altforeground
  , base03
  , base02
  , base01
  , base00
  , base0
  , base1
  , base2
  , base3
  , yellow  
  , orange  
  , red     
  , magenta 
  , violet  
  , blue    
  , cyan    
  , green   
  , light
  , lightred
  , lightorange
  , lightcyan
  , shadow
  , shadoworange
  , white
  , black
  )
where

import Prelude
import Data.Text (Text,pack)
import Numeric (readHex)
import Data.List (intercalate)

cssGradient :: String -> String -> Text
cssGradient from to = 
  pack $ "background: rgb("++rgbfrom++");" ++
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

cssVerticalGradient from to =
  pack $
    "background: rgb("++rgbfrom++"); /* Old browsers */" ++
    "background: -moz-linear-gradient(top,  rgba("++rgbfrom++",1) 0%, rgba("++rgbto++",1) 100%); /* FF3.6+ */" ++
    "background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,rgba("++rgbfrom++",1)), color-stop(100%,rgba("++rgbto++",1))); /* Chrome,Safari4+ */" ++
    "background: -webkit-linear-gradient(top,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* Chrome10+,Safari5.1+ */" ++
    "background: -o-linear-gradient(top,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* Opera 11.10+ */" ++
    "background: -ms-linear-gradient(top,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* IE10+ */" ++
    "background: linear-gradient(to bottom,  rgba("++rgbfrom++",1) 0%,rgba("++rgbto++",1) 100%); /* W3C */" ++
    "filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='"++from++"', endColorstr='"++to++"',GradientType=0 ); /* IE6-9 */"
  where
      rgbfrom = cssToRgb from
      rgbto   = cssToRgb to




background , foreground , altbackground , altforeground , base03 , base02 , base01 , base00 , base0 , base1 , base2 , base3 , yellow  , orange  , red     , magenta , violet  , blue    , cyan    , green   , lightred , lightorange , lightcyan , shadoworange , white , black :: String

background=base3
foreground=base1 
altbackground=base01
altforeground=base1

base03="#002b36"
base02="#073642"
base01="#586e75"
base00="#657b83"
base0="#839496"
base1="#93a1a1"
base2="#eee8d5"
base3="#fdf6e3"

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

lightred = light red
lightorange = light orange
lightcyan = light cyan

shadow :: String -> String
shadow color = "rgba("++(cssToRgb color)++",0.5)"
shadoworange = shadow orange

white="rgba(255,255,255,0.6)"
black="rgba(0,43,54,0.6)"


-- Hexa style colors                                                            
scalarFromHex :: String ->  Int
scalarFromHex = fst . head . readHex
                                                                                
-- | Color from CSS style color string                                          
cssToRgb :: String -> String                                                     
cssToRgb ('#':rd:ru:gd:gu:bd:bu:[]) = intercalate "," $
  map (show . scalarFromHex) [[rd,ru],[gd,gu],[bd,bu]]
cssToRgb ('#':r:g:b:[]) = cssToRgb ['#',r,r,g,g,b,b]                            
cssToRgb _ = error "Bad color!!!!"
