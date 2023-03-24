module StyleLabels exposing (buttonLabel, layoutWithHeader, linkLabel)

import Colorscheme
import Element exposing (Attribute, Element, column, el, fill, layout, link, mouseOver, padding, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


buttonLabel : String -> List (Attribute msg) -> Element msg
buttonLabel txt attrs =
    el
        (attrs
            ++ [ Font.color Colorscheme.light.primary
               , Background.color Colorscheme.light.bg
               , padding 10
               , Border.rounded 8
               , mouseOver
                    [ Font.color Colorscheme.light.bg
                    , Background.color Colorscheme.light.primary
                    ]
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


header : Element msg
header =
    row []
        [ link [] { url = "/", label = linkLabel "Home" [] }
        , link [] { url = "/menu", label = linkLabel "Menu" [] }
        , link [] { url = "/login", label = linkLabel "Login" [] }
        , link [] { url = "/adminCategories", label = linkLabel "Manage categories" [] }
        , link [] { url = "/adminProducts", label = linkLabel "Manage products" [] }
        ]


layoutWithHeader : List (Attribute msg) -> Element msg -> Html msg
layoutWithHeader attrs body =
    layout attrs <|
        column [ width fill ] [ header, body ]
