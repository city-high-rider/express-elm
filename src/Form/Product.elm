{-
   If you are thinking : "Wow, that's a lot of duplicate code!",
   I agree!

   Go look at the comment in src/Form/Category.elm for an explanation!
-}


module Form.Product exposing (..)

import Category exposing (Category, CategoryId, catIdToString)
import Form.Category exposing (catsToOptions)
import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Products exposing (Product)


categoryForm : List Category -> Product -> (Maybe Product -> msg) -> Html msg
categoryForm cats oldProduct msg =
    div []
        [ div []
            [ text ""
            , br [] []
            , input
                [ type_ "text"
                , value oldProduct.name
                , onInput (msg << Just << updateName oldProduct)
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
                , onInput (msg << Just << updateDescription oldProduct)
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


updateCost : Product -> String -> Maybe Product
updateCost oldProd newCost =
    case String.toInt newCost of
        Nothing ->
            Nothing

        Just n ->
            Just { oldProd | price = n }


updateSize : Product -> String -> Maybe Product
updateSize oldProd newSize =
    case String.toInt newSize of
        Nothing ->
            Nothing

        Just n ->
            Just { oldProd | size = n }


updateCategory : Product -> String -> Maybe Product
updateCategory oldProd newCat =
    case Maybe.map Category.intToCatId (String.toInt newCat) of
        Nothing ->
            Nothing

        Just newId ->
            Just { oldProd | category = newId }
