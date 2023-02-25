module Category exposing (..)

import Json.Decode exposing (Decoder, int)
import Url.Parser exposing (Parser, custom)


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
