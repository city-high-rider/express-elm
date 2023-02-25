module Category exposing (..)

import Json.Decode exposing (Decoder, int, string, list, int)
import Url.Parser exposing (Parser, custom)
import Json.Decode.Pipeline exposing (required)

type alias Category =
    { id : CategoryId
    , name : String
    , units : String
    }


type CategoryId
    = CategoryId Int


catIdToInt : CategoryId -> Int
catIdToInt (CategoryId id) =
    id


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
