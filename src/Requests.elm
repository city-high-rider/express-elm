module Requests exposing (..)

import Category exposing (Category, catIdToString)
import Http exposing (..)
import Json.Encode as Encode
import Products exposing (Product)
import ServerResponse exposing (..)


reqWithPass : Encode.Value -> String -> Http.Body
reqWithPass stuff pass =
    Http.jsonBody <|
        Encode.object
            [ ( "request", stuff )
            , ( "password", Encode.string pass )
            ]


expectServerResponse : (Result Http.Error ServerResponse -> msg) -> Http.Expect msg
expectServerResponse msg =
    Http.expectJson msg responseDecoder


submitResult : String -> (Result Http.Error ServerResponse -> msg) -> Category -> Cmd msg
submitResult pass msg cat =
    Http.post
        { url = "http://localhost:3000/newCat"
        , body = reqWithPass (Category.newCatEncoder cat) pass
        , expect = expectServerResponse msg
        }


submitProduct : String -> (Result Http.Error ServerResponse -> msg) -> Product -> Cmd msg
submitProduct pass msg prod =
    Http.post
        { url = "http://localhost:3000/newProd"
        , body = reqWithPass (Products.newProductEncoder prod) pass
        , expect = expectServerResponse msg
        }


removeProduct : String -> (Result Http.Error ServerResponse -> msg) -> Int -> Cmd msg
removeProduct pass msg id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/deleteProd/" ++ String.fromInt id
        , body = reqWithPass Encode.null pass
        , expect = expectServerResponse msg
        , timeout = Nothing
        , tracker = Nothing
        }


editProduct : String -> (Result Http.Error ServerResponse -> msg) -> Product -> Int -> Cmd msg
editProduct pass msg newProd id =
    Http.post
        { url = "http://localhost:3000/updateProd/" ++ String.fromInt id
        , body = reqWithPass (Products.newProductEncoder newProd) pass
        , expect = expectServerResponse msg
        }


deleteCat : String -> (Result Http.Error ServerResponse -> msg) -> Category.CategoryId -> Cmd msg
deleteCat pass msg id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/deleteCat/" ++ catIdToString id
        , body = reqWithPass Encode.null pass
        , expect = expectServerResponse msg
        , timeout = Nothing
        , tracker = Nothing
        }


updateCat : String -> (Result Http.Error ServerResponse -> msg) -> Category -> Cmd msg
updateCat pass msg cat =
    Http.post
        { url = "http://localhost:3000/updateCat/" ++ catIdToString cat.id
        , body = reqWithPass (Category.newCatEncoder cat) pass
        , expect = expectServerResponse msg
        }


checkPassword : (Result Http.Error ServerResponse -> msg) -> String -> Cmd msg
checkPassword msg pass =
    Http.post
        { url = "http://localhost:3000/checkPass/" ++ pass
        , body = Http.emptyBody
        , expect = expectServerResponse msg
        }
