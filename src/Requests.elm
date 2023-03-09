module Requests exposing (..)

import Category exposing (Category, catIdToString)
import Http exposing (..)
import Products exposing (Product)


submitResult : (String -> Result Http.Error String -> msg) -> Category -> Cmd msg
submitResult msg cat =
    Http.post
        { url = "http://localhost:3000/newCat"
        , body = Http.jsonBody (Category.newCatEncoder cat)
        , expect = Http.expectString (msg "created category")
        }


submitProduct : (Result Http.Error String -> msg) -> Product -> Cmd msg
submitProduct msg prod =
    Http.post
        { url = "http://localhost:3000/newProd"
        , body = Http.jsonBody (Products.newProductEncoder prod)
        , expect = Http.expectString msg
        }


removeProduct : (Result Http.Error String -> msg) -> Int -> Cmd msg
removeProduct msg id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/deleteProd/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectString msg
        , timeout = Nothing
        , tracker = Nothing
        }


editProduct : (Result Http.Error String -> msg) -> Product -> Int -> Cmd msg
editProduct msg newProd id =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:3000/updateProd/" ++ String.fromInt id
        , body = Http.jsonBody (Products.newProductEncoder newProd)
        , expect = Http.expectString msg
        , timeout = Nothing
        , tracker = Nothing
        }


deleteCat : (String -> Result Http.Error String -> msg) -> Category.CategoryId -> Cmd msg
deleteCat msg id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/deleteCat/" ++ catIdToString id
        , body = Http.emptyBody
        , expect = Http.expectString (msg "deleted category")
        , timeout = Nothing
        , tracker = Nothing
        }


updateCat : (String -> Result Http.Error String -> msg) -> Category -> Cmd msg
updateCat msg cat =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:3000/updateCat/" ++ catIdToString cat.id
        , body = Http.jsonBody (Category.newCatEncoder cat)
        , expect = Http.expectString (msg "updated category")
        , timeout = Nothing
        , tracker = Nothing
        }


checkPassword : (Result Http.Error String -> msg) -> String -> Cmd msg
checkPassword msg pass =
    Http.post
        { url = "http://localhost:3000/checkPass/" ++ pass
        , body = Http.emptyBody
        , expect = Http.expectString msg
        }
