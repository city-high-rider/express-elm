module Pages.Menu exposing (..)

import Html exposing (..)
import Http
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (WebData)
import Url.Parser exposing (Parser, custom)



-- init and model


type alias Model =
    { products : WebData (List Product)
    }


type alias Product =
    { id : Int
    , name : String
    , description : String
    , size : Int
    , price : Int
    , category : CategoryId
    }


init : CategoryId -> ( Model, Cmd Msg )
init startingCategory =
    ( Model RemoteData.Loading
    , getCategory startingCategory
    )



-- view function

view : Model -> Html Msg
view model =
    case model.products of 
        RemoteData.NotAsked ->
            div [] [h2 [] [text "You haven't asked for the data!"]]

        RemoteData.Loading ->
            h2 [] [text "Loading.. please wait"]

        RemoteData.Failure err ->
            div []
            [ h2 [] [text "someting went wrong"]
            , viewError err
            ]

        RemoteData.Success prods ->
            viewProds prods


viewError : Http.Error -> Html Msg
viewError err =
    case err of 
        Http.BadUrl s ->
            p [] [text s]

        Http.Timeout ->
            p [] [text "time out"]

        Http.NetworkError ->
            p [] [text "network error"]

        Http.BadStatus s ->
            p [] [text (String.fromInt s)]

        Http.BadBody s ->
            p [] [text s]


viewProds : List Product -> Html Msg
viewProds prods = 
    div [] (List.map viewProd prods)


viewProd : Product -> Html Msg
viewProd product =
    let
        productCost = String.fromFloat <| ( toFloat product.price / 100)
    in
    div []
    [ h3 [] [text product.name]
    , p [] [text ("Description: " ++ product.description)]
    , p [] [text ("Cost : " ++ productCost)]
    , p [] [text (String.fromInt product.size ++ " mL")]
    ]
-- update function

-- TODO : Change this request to send to a different url
-- and update the server to send out categories at that url
getCategory : CategoryId -> Cmd Msg
getCategory (CategoryId id) =
    Http.get
        { url = "http://localhost:3000/queryDb"
        , expect = Http.expectJson (RemoteData.fromResult >> GotProducts) productsDecoder
        }


type Msg
    = GotProducts (WebData (List Product))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts prods ->
            ( { model | products = prods }, Cmd.none )


type CategoryId
    = CategoryId Int


catIdToInt : CategoryId -> Int
catIdToInt ( CategoryId id ) = id

-- decoders


menuRouteParser : Parser (CategoryId -> a) a
menuRouteParser =
    custom "CATEGORYID" <|
        \catId -> Maybe.map CategoryId (String.toInt catId)


productsDecoder : Decoder (List Product)
productsDecoder =
    list productDecoder


productDecoder : Decoder Product
productDecoder =
    Json.Decode.succeed Product
        |> required "id" int
        |> required "name" string
        |> required "description" string
        |> required "size" int
        |> required "price_cents" int
        |> required "category" catIdDecoder


catIdDecoder : Decoder CategoryId
catIdDecoder =
    Json.Decode.map CategoryId int
