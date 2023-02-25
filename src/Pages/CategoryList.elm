module Pages.CategoryList exposing (..)

import Category exposing (CategoryId, catIdDecoder, catIdToInt)
import ErrorViewing exposing (viewHttpError)
import Html exposing (Html, a, div, h2, h3, p, text)
import Html.Attributes exposing (href)
import Http
import Json.Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (WebData)


type alias Model =
    { categories : WebData (List Category)
    }


type alias Category =
    { id : CategoryId
    , name : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model RemoteData.Loading, getCategories )



-- view


view : Model -> Html Msg
view model =
    case model.categories of
        RemoteData.NotAsked ->
            p [] [ text "You haven't asked for anything" ]

        RemoteData.Loading ->
            h3 [] [ text "loading.. please wait" ]

        RemoteData.Failure err ->
            viewHttpError err

        RemoteData.Success cats ->
            div []
                [ h2 [] [ text "Available food categories:" ]
                , showCategories cats
                ]


showCategories : List Category -> Html Msg
showCategories cats =
    div []
        (List.map showCategory cats)


showCategory : Category -> Html Msg
showCategory cat =
    let
        idNumber =
            (String.fromInt << catIdToInt) cat.id
    in
    a [ href ("menu/" ++ idNumber) ] [ text cat.name ]



-- update


type Msg
    = GotCats (WebData (List Category))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCats result ->
            ( { model | categories = result }
            , Cmd.none
            )


getCategories : Cmd Msg
getCategories =
    Http.get
        { url = "http://localhost:3000/categories"
        , expect = Http.expectJson (RemoteData.fromResult >> GotCats) catsDecoder
        }


catsDecoder : Decoder (List Category)
catsDecoder =
    list catDecoder


catDecoder : Decoder Category
catDecoder =
    Json.Decode.succeed Category
        |> required "id" catIdDecoder
        |> required "name" string
