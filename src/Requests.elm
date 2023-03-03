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


submitProduct : (String -> Result Http.Error String -> msg) -> Product -> Cmd msg
submitProduct msg prod =
    Http.post
        { url = "http://localhost:3000/newProd"
        , body = Http.jsonBody (Products.newProductEncoder prod)
        , expect = Http.expectString (msg "created product")
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
