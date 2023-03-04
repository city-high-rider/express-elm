module Products exposing (..)

import Category exposing (CategoryId, catIdDecoder)
import Http
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (WebData)


type alias Product =
    { id : Int
    , name : String
    , description : String
    , size : Int
    , price : Int
    , category : CategoryId
    }


type alias UserInputProduct =
    { name : String
    , description : String
    , size : String
    , price : String
    , category : String
    }


empty : UserInputProduct
empty =
    UserInputProduct
        ""
        ""
        ""
        ""
        ""


userInputToProduct : UserInputProduct -> Result String Product
userInputToProduct userInput =
    let
        processSize =
            Result.fromMaybe "Size must be integer!" (String.toInt userInput.size)
                |> Result.andThen checkPositive

        processCost =
            Result.fromMaybe "Cost must be integer!" (String.toInt userInput.price)
                |> Result.andThen checkPositive

        processCategory =
            Result.fromMaybe "Select a category!" (String.toInt userInput.category)
                |> Result.andThen checkPositive
    in
    case allOk [ processSize, processCost, processCategory ] of
        Err e ->
            Err e

        Ok (s :: c :: cat :: []) ->
            Ok <|
                Product
                    -1
                    userInput.name
                    userInput.description
                    s
                    c
                    (Category.intToCatId cat)

        _ ->
            Err "Not enough information"


prodToString : Product -> UserInputProduct
prodToString prod =
    UserInputProduct
        prod.name
        prod.description
        (String.fromInt prod.size)
        (String.fromInt prod.price)
        (Category.catIdToString prod.category)


allOk : List (Result e a) -> Result e (List a)
allOk toCheck =
    let
        fn : Result e a -> Result e (List a) -> Result e (List a)
        fn element accumulator =
            case ( element, accumulator ) of
                ( Ok e, Ok xs ) ->
                    Ok (xs ++ [ e ])

                ( Err e, Ok _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e
    in
    List.foldl fn (Ok []) toCheck


checkPositive : Int -> Result String Int
checkPositive n =
    if n <= 0 then
        Err "Input must be positive and non zero!"

    else
        Ok n



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


newProductEncoder : Product -> Encode.Value
newProductEncoder prod =
    Encode.object
        [ ( "name", Encode.string prod.name )
        , ( "description", Encode.string prod.description )
        , ( "size", Encode.int prod.size )
        , ( "price_cents", Encode.int prod.price )
        , ( "category", Encode.int (Category.catIdToInt prod.category) )
        ]



-- requests


getProductsById : (Category.CategoryId -> WebData (List Product) -> msg) -> CategoryId -> Cmd msg
getProductsById msg catId =
    Http.get
        { url = "http://localhost:3000/menu/" ++ (String.fromInt <| Category.catIdToInt catId)
        , expect = Http.expectJson (RemoteData.fromResult >> msg catId) productsDecoder
        }


getProducts : (WebData (List Product) -> msg) -> Cmd msg
getProducts msg =
    Http.get
        { url = "http://localhost:3000/products"
        , expect = Http.expectJson (RemoteData.fromResult >> msg) productsDecoder
        }
