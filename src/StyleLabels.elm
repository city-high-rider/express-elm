module StyleLabels exposing (buttonLabel)

import Colorscheme
import Element exposing (Attribute, Element, column, el, padding, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


buttonLabel : String -> List Attribute -> Element msg
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
