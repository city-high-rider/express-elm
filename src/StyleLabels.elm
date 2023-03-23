module StyleLabels exposing (buttonLabel, linkLabel)

import Colorscheme
import Element exposing (Attribute, Element, column, el, mouseOver, padding, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


buttonLabel : String -> List (Attribute msg) -> Element msg
buttonLabel txt attrs =
    el
        (attrs
            ++ [ Font.color Colorscheme.light.primary
               , Background.color Colorscheme.light.bg
               , padding 10
               , Border.rounded 8
               ]
        )
        (text txt)


linkLabel : String -> List (Attribute msg) -> Element msg
linkLabel txt attrs =
    el
        (attrs
            ++ [ Font.color Colorscheme.light.secondary
               , mouseOver [ Font.color Colorscheme.light.misc ]
               ]
        )
        (text txt)
