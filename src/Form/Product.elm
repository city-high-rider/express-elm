{-
   If you are thinking : "Wow, that's a lot of duplicate code!",
   I agree!

   Go look at the comment in src/Form/Category.elm for an explanation!
-}


module Form.Product exposing (..)

import Category exposing (Category)
import Element exposing (Element, column, text)
import Element.Input as Input exposing (labelAbove, option)
import Form.Category exposing (catsToOptions)
import Html exposing (Html, option)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Products exposing (Product, UserInputProduct)


productForm : List Category -> UserInputProduct -> (UserInputProduct -> msg) -> Element msg
productForm cats oldProduct msg =
    column []
        [ Input.text []
            { onChange = msg << updateName oldProduct
            , text = oldProduct.name
            , placeholder = Nothing
            , label = labelAbove [] (text "Product name")
            }
        , Input.text []
            { onChange = msg << updateSize oldProduct
            , text = oldProduct.size
            , placeholder = Nothing
            , label = labelAbove [] (text "Size of product")
            }
        , Input.text []
            { onChange = msg << updateDescription oldProduct
            , text = oldProduct.description
            , placeholder = Nothing
            , label = labelAbove [] (text "Description")
            }
        , Input.text []
            { onChange = msg << updateCost oldProduct
            , text = oldProduct.price
            , placeholder = Nothing
            , label = labelAbove [] (text "Cost in cents")
            }
        , Input.radio []
            { onChange = msg << updateCategory oldProduct
            , options = List.map (\c -> Input.option (Category.catIdToString c.id) <| text c.name) cats
            , selected = Nothing
            , label = labelAbove [] (text "Category the product belongs to")
            }
        ]



{-
   div []
       [ div []
           [ text "Product name"
           , br [] []
           , input
               [ type_ "text"
               , value oldProduct.name
               , onInput (msg << updateName oldProduct)
               ]
               []
           ]
       , br [] []
       , div []
           [ text "Size"
           , br [] []
           , input
               [ type_ "text"
               , value oldProduct.size
               , onInput (msg << updateSize oldProduct)
               ]
               []
           ]
       , br [] []
       , div []
           [ text "Description"
           , br [] []
           , input
               [ type_ "text"
               , value oldProduct.description
               , onInput (msg << updateDescription oldProduct)
               ]
               []
           ]
       , br [] []
       , div []
           [ text "Cost in cents"
           , br [] []
           , input
               [ type_ "text"
               , value oldProduct.price
               , onInput (msg << updateCost oldProduct)
               ]
               []
           ]
       , br [] []
       , div []
           [ text "Category that it belongs to"
           , br [] []
           , select [ onInput (msg << updateCategory oldProduct) ]
               (defaultOption :: catsToOptions cats)
           ]
       , br [] []
       ]
-}


defaultOption : Html msg
defaultOption =
    Html.option [ value "Nothing" ] [ Html.text "Select..." ]


updateName : UserInputProduct -> String -> UserInputProduct
updateName oldProd newName =
    { oldProd | name = newName }


updateDescription : UserInputProduct -> String -> UserInputProduct
updateDescription oldProd newDescription =
    { oldProd | description = newDescription }


updateCost : UserInputProduct -> String -> UserInputProduct
updateCost oldProd newCost =
    { oldProd | price = newCost }


updateSize : UserInputProduct -> String -> UserInputProduct
updateSize oldProd newSize =
    { oldProd | size = newSize }


updateCategory : UserInputProduct -> String -> UserInputProduct
updateCategory oldProd newCat =
    { oldProd | category = newCat }


prodsToOptions : List Product -> List (Html msg)
prodsToOptions prods =
    List.map (\p -> Html.option [ value <| String.fromInt p.id ] [ Html.text p.name ]) prods
