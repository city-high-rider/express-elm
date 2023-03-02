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
import Products exposing (Product)


productForm : List Category -> Product -> (Result String Product -> msg) -> Html msg
productForm cats oldProduct msg =
    div []
        [ div []
            [ text "Product name"
            , br [] []
            , input
                [ type_ "text"
                , value oldProduct.name
                , onInput (msg << Ok << updateName oldProduct)
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Size"
            , br [] []
            , input
                [ type_ "text"
                , value (String.fromInt oldProduct.size)
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
                , onInput (msg << Ok << updateDescription oldProduct)
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Cost in cents"
            , br [] []
            , input
                [ type_ "text"
                , value (String.fromInt oldProduct.price)
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


updateName : Product -> String -> Product
updateName oldProd newName =
    { oldProd | name = newName }


updateDescription : Product -> String -> Product
updateDescription oldProd newDescription =
    { oldProd | description = newDescription }


updateCost : Product -> String -> Result String Product
updateCost oldProd newCost =
    let
        value =
            Result.fromMaybe "Invalid cost! Must be an integer." (String.toInt newCost)
                |> Result.andThen checkPositive
    in
    case value of
        Err e ->
            Err e

        Ok n ->
            Ok { oldProd | price = n }


updateSize : Product -> String -> Result String Product
updateSize oldProd newSize =
    let
        value =
            Result.fromMaybe "Invalid size! Must be an integer." (String.toInt newSize)
                |> Result.andThen checkPositive
    in
    case value of
        Err e ->
            Err e

        Ok n ->
            Ok { oldProd | size = n }


updateCategory : Product -> String -> Result String Product
updateCategory oldProd newCat =
    case Maybe.map Category.intToCatId (String.toInt newCat) of
        Nothing ->
            Err "Select a category!"

        Just newId ->
            Ok { oldProd | category = newId }


checkPositive : Int -> Result String Int
checkPositive n =
    if n <= 0 then
        Err "Input must be positive and non zero!"

    else
        Ok n
