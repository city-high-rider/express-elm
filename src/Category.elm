module Category exposing (..)

import Http
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Url.Parser exposing (Parser, custom)


type alias Category =
    { id : CategoryId
    , name : String
    , units : String
    }


empty : Category
empty =
    Category (CategoryId -1) "" ""


type CategoryId
    = CategoryId Int


catIdToInt : CategoryId -> Int
catIdToInt (CategoryId id) =
    id


catIdToString : CategoryId -> String
catIdToString id = String.fromInt <| catIdToInt id

intToCatId : Int -> CategoryId
intToCatId x =
    CategoryId x


emptyCatId : CategoryId
emptyCatId =
    CategoryId -1


catIdDecoder : Decoder CategoryId
catIdDecoder =
    Json.Decode.map CategoryId int


categoryParser : Parser (CategoryId -> a) a
categoryParser =
    custom "CATEGORYID" <|
        \catId -> Maybe.map CategoryId (String.toInt catId)


catsDecoder : Decoder (List Category)
catsDecoder =
    list catDecoder


catDecoder : Decoder Category
catDecoder =
    Json.Decode.succeed Category
        |> required "id" catIdDecoder
        |> required "name" string
        |> required "sizeunits" string


newCatEncoder : Category -> Encode.Value
newCatEncoder cat =
    Encode.object
        [ ( "name", Encode.string cat.name )
        , ( "units", Encode.string cat.units )
        ]


getCategories : (WebData (List Category) -> msg) -> Cmd msg
getCategories msg =
    Http.get
        { url = "http://localhost:3000/categories"
        , expect = Http.expectJson (RemoteData.fromResult >> msg) catsDecoder
        }


verifyCat : Category -> Result String Category
verifyCat cat =
    if cat.name == "" then
        Err "Name musn't be empty!"
    else if cat.units == "" then
        Err "Must have units!"
    else
        Ok cat
