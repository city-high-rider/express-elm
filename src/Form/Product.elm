{-
   If you are thinking : "Wow, that's a lot of duplicate code!",
   I agree!

   Go look at the comment in src/Form/Category.elm for an explanation!
-}


module Form.Product exposing (..)

import Category exposing (Category)
import Form.Category exposing (catsToOptions)
import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Products exposing (Product, UserInputProduct)


productForm : List Category -> UserInputProduct -> (UserInputProduct -> msg) -> Html msg
productForm cats oldProduct msg =
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


defaultOption : Html msg
defaultOption =
    option [ value "Nothing" ] [ text "Select..." ]


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


