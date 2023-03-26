module StyleLabels exposing (buttonLabel, layoutWithHeader, linkLabel)

import Colorscheme
import Element exposing (Attribute, Element, centerX, column, el, fill, layout, link, mouseOver, padding, row, spacing, text, width)
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
    row
        [ padding 10
        , Background.color Colorscheme.light.fgDarker
        , width fill
        , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 16, bottomRight = 16 }
        ]
        [ row [ centerX, spacing 20 ]
            [ link [] { url = "/", label = linkLabel "Home" [] }
            , link [] { url = "/menu", label = linkLabel "Menu" [] }
            , link [] { url = "/login", label = linkLabel "Login" [] }
            , link [] { url = "/adminCategories", label = linkLabel "Manage categories" [] }
            , link [] { url = "/adminProducts", label = linkLabel "Manage products" [] }
            , link [] { url = "/orders", label = linkLabel "Manage orders" [] }
            ]
        ]


layoutWithHeader : List (Attribute msg) -> Element msg -> Html msg
layoutWithHeader attrs body =
    layout attrs <|
        column [ width fill ] [ header, body ]
