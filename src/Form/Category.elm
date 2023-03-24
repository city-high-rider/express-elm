{-
   Note:
   Elm does not support dynamically updating records.
   this means that there is no way to pass a field name to a function and have
   the function update a record with that field name.

   That means we must bite the bullet and write a lot of setter functions, and
   that we can't make our code regarding forms and record super abstract.

   The best that we can do is move it to its own modules, which will clear up the
   code for pages which require a form like this.

   And now, instead of having a message for updating every possible field of
   a record, we just have a singular message that works with the entire record.
   Every time our form gets input, we modify the old record to return a new one
   which is what we send as the payload for the message

   tl;dr: there is no way in elm to get around writing a lot of repetitive setter
   functions like you see below, and in the product form.

   See this thread: https://discourse.elm-lang.org/t/dynamic-record-updates/1198
-}


module Form.Category exposing (..)

import Category exposing (Category)
import Element exposing (Element, column, text)
import Element.Input as Input exposing (labelAbove)
import Html exposing (Html, br, div, input, option)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


categoryForm : Category -> (Category -> msg) -> Element msg
categoryForm oldCategory msg =
    column []
        [ Input.text []
            { onChange = msg << updateName oldCategory
            , text = oldCategory.name
            , placeholder = Nothing
            , label = labelAbove [] (text "Category name")
            }
        , Input.text []
            { onChange = msg << updateUnits oldCategory
            , text = oldCategory.units
            , placeholder = Nothing
            , label = labelAbove [] (text "Units of measurement")
            }
        ]


catsToOptions : List Category -> List (Html msg)
catsToOptions cats =
    List.map catToOption cats


catToOption : Category -> Html msg
catToOption cat =
    option [ value (String.fromInt <| Category.catIdToInt cat.id) ] [ Html.text cat.name ]


updateName : Category -> String -> Category
updateName oldCat newName =
    { oldCat | name = newName }


updateUnits : Category -> String -> Category
updateUnits oldCat newUnits =
    { oldCat | units = newUnits }
