module Products exposing (..)

import Category exposing (CategoryId, catIdDecoder)
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Product =
    { id : Int
    , name : String
    , description : String
    , size : Int
    , price : Int
    , category : CategoryId
    }


empty : Product
empty =
    Product
        -1
        ""
        ""
        0
        0
        (Category.intToCatId -1)

-- decoders


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
