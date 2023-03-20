module Colorscheme exposing (light)

import Element exposing (Color, rgb255)


type alias Colorscheme =
    { primary : Color
    , secondary : Color
    , fg : Color
    , bg : Color
    , misc : List Color
    }


light : Colorscheme
light =
    Colorscheme
        (rgb255 236 102 109)
        (rgb255 254 119 29)
        (rgb255 246 233 199)
        (rgb255 116 113 123)
        [ rgb255 71 152 136 ]
