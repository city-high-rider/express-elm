module Pages.Menu exposing (..)

import Category exposing (Category, CategoryId, getCategories)
import ErrorViewing exposing (viewHttpError)
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onCheck, onClick)
import Products exposing (Product, getProductsById)
import RemoteData exposing (WebData)



-- init and model


type alias Order =
    ( Product, Int )


type alias Model =
    { categories : WebData (List Category) -- these are the available categories to pick food from
    , sections : List Section -- list of sections that are currently displayed
    , cart : List Order
    }



-- you will see categories referred to as "cats" extensively
{- A section consists of a category and its corresponding food items -}


type alias Section =
    { category : Category
    , products : WebData (List Product)
    }


init : ( Model, Cmd Msg )
init =
    ( Model RemoteData.Loading [] []
    , getCategories GotCats
    )



-- view function


view : Model -> Html Msg
view model =
    div []
        [ viewChecks model
        , viewSections model.sections
        ]


viewChecks : Model -> Html Msg
viewChecks model =
    case model.categories of
        RemoteData.NotAsked ->
            div [] [ h2 [] [ text "You haven't asked for the data!" ] ]

        RemoteData.Loading ->
            h2 [] [ text "Loading.. please wait" ]

        RemoteData.Failure err ->
            div []
                [ h2 [] [ text "someting went wrong" ]
                , viewHttpError err
                ]

        RemoteData.Success cats ->
            div []
                [ makeCheckmarks cats
                , viewCart model.cart
                ]


viewSections : List Section -> Html Msg
viewSections sections =
    div [] (List.map viewSection sections)


viewSection : Section -> Html Msg
viewSection section =
    div []
        [ h2 [] [ text section.category.name ]
        , viewProds section.products section.category.units
        ]


makeCheckmarks : List Category -> Html Msg
makeCheckmarks cats =
    div []
        (List.map makeCheckmark cats)


makeCheckmark : Category -> Html Msg
makeCheckmark cat =
    div []
        [ span [] [ text cat.name ]
        , input [ type_ "checkbox", onCheck (CheckedCat cat) ] []
        ]


viewCart : List Order -> Html Msg
viewCart orders =
    case orders of
        [] ->
            p [] [ text "You haven't ordered anything yet!" ]

        ords ->
            div []
                [ h3 [] [ text "You have ordered:" ]
                , viewOrders ords
                , p []
                    [ text <|
                        "Your total comes out to $"
                            ++ (String.fromFloat <|
                                    (\x -> x / 100) <|
                                        toFloat <|
                                            List.sum <|
                                                List.map (\( p, q ) -> p.price * q) ords
                               )
                    ]
                ]


viewOrders : List Order -> Html Msg
viewOrders orders =
    ul [] (List.map viewOrder orders)


viewOrder : Order -> Html Msg
viewOrder ( prod, qty ) =
    div []
        [ li [] [ text (String.fromInt qty ++ "x " ++ prod.name) ]
        , button [ onClick <| AddOrder ( prod, -1 ) ] [ text "-" ]
        ]


viewProds : WebData (List Product) -> String -> Html Msg
viewProds prods units =
    case prods of
        RemoteData.NotAsked ->
            p [] [ text "haven't asked" ]

        RemoteData.Loading ->
            p [] [ text "Getting your items... please wait" ]

        RemoteData.Failure err ->
            div []
                [ h2 [] [ text "unable to get your item!" ]
                , viewHttpError err
                ]

        RemoteData.Success goodProds ->
            div [] (List.map (viewProd units) goodProds)


viewProd : String -> Product -> Html Msg
viewProd units product =
    let
        productCost =
            String.fromFloat <| (toFloat product.price / 100)
    in
    div []
        [ h3 [] [ text product.name ]
        , p [] [ text ("Description: " ++ product.description) ]
        , p [] [ text ("Cost : " ++ productCost) ]
        , p [] [ text (String.fromInt product.size ++ units) ]
        , button [ onClick (AddOrder ( product, 1 )) ] [ text "Add to cart" ]
        ]



-- update function


type Msg
    = GotProducts CategoryId (WebData (List Product))
    | GotCats (WebData (List Category))
    | CheckedCat Category Bool
    | AddOrder Order


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts forCat prods ->
            ( { model | sections = insertProducts prods forCat model.sections }, Cmd.none )

        GotCats cats ->
            ( { model | categories = cats }, Cmd.none )

        CheckedCat cat on ->
            let
                newSections =
                    if on then
                        Section cat RemoteData.Loading :: model.sections

                    else
                        List.filter (\s -> s.category /= cat) model.sections
            in
            ( { model
                | sections = newSections
              }
            , updateSections newSections
            )

        AddOrder order ->
            ( { model | cart = mergeOrder model.cart order }, Cmd.none )


insertProducts : WebData (List Product) -> CategoryId -> List Section -> List Section
insertProducts newProducts id oldSections =
    List.map
        (\s ->
            if s.category.id == id then
                { s | products = newProducts }

            else
                s
        )
        oldSections


mergeOrder : List Order -> Order -> List Order
mergeOrder orders ( prod, qty ) =
    case orders of
        [] ->
            [ ( prod, qty ) ]

        ( cp, cq ) :: xs ->
            if cp == prod && cq + qty > 0 then
                ( cp, cq + qty ) :: xs

            else if cq + qty <= 0 then
                xs

            else
                ( cp, cq ) :: mergeOrder xs ( prod, qty )


updateSections : List Section -> Cmd Msg
updateSections sections =
    List.map updateSection sections
        |> Cmd.batch


updateSection : Section -> Cmd Msg
updateSection section =
    getProductsById GotProducts section.category.id
