{-
   If you are thinking : "Wow, that's a lot of duplicate code!",
   I agree!

   Go look at the comment in src/Form/Category.elm for an explanation!
-}


module Form.Product exposing (..)

import Category exposing (Category, CategoryId)
import Element exposing (Element, column, text)
import Element.Input as Input exposing (labelAbove)
import Html exposing (Html)
import Html.Attributes exposing (value)
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
            , options = List.map (\c -> Input.option c.id <| text c.name) cats
            , selected = oldProduct.category
            , label = labelAbove [] (text "Category the product belongs to")
            }
        ]


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


updateCategory : UserInputProduct -> CategoryId -> UserInputProduct
updateCategory oldProd newCat =
    { oldProd | category = Just newCat }
