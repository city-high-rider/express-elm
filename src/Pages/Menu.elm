module Pages.Menu exposing (..)

import Category exposing (CategoryId)
import ErrorViewing exposing (viewHttpError)
import Html exposing (..)
import Http
import Products exposing (Product, productsDecoder)
import RemoteData exposing (WebData)



-- init and model


type alias Model =
    { products : WebData (List Product)
    }


init : CategoryId -> ( Model, Cmd Msg )
init startingCategory =
    ( Model RemoteData.Loading
    , getProducts startingCategory
    )



-- view function


view : Model -> Html Msg
view model =
    case model.products of
        RemoteData.NotAsked ->
            div [] [ h2 [] [ text "You haven't asked for the data!" ] ]

        RemoteData.Loading ->
            h2 [] [ text "Loading.. please wait" ]

        RemoteData.Failure err ->
            div []
                [ h2 [] [ text "someting went wrong" ]
                , viewHttpError err
                ]

        RemoteData.Success prods ->
            viewProds prods


viewProds : List Product -> Html Msg
viewProds prods =
    div [] (List.map viewProd prods)


viewProd : Product -> Html Msg
viewProd product =
    let
        productCost =
            String.fromFloat <| (toFloat product.price / 100)
    in
    div []
        [ h3 [] [ text product.name ]
        , p [] [ text ("Description: " ++ product.description) ]
        , p [] [ text ("Cost : " ++ productCost) ]
        , p [] [ text (String.fromInt product.size ++ " mL") ]
        ]



-- update function


getProducts : CategoryId -> Cmd Msg
getProducts catId =
    Http.get
        { url = "http://localhost:3000/menu/" ++ (String.fromInt <| Category.catIdToInt catId)
        , expect = Http.expectJson (RemoteData.fromResult >> GotProducts) productsDecoder
        }


type Msg
    = GotProducts (WebData (List Product))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts prods ->
            ( { model | products = prods }, Cmd.none )
