module Pages.Home exposing (view)
import Html exposing (text, div, Html, p, h2, a)
import Html.Attributes exposing (href)

view : Html msg
view =
    div []
    [ h2 [] [text "Welcome to the website!"]
    , p [] [text "This is a test to try and read and display some information from a database, using an expressjs backend."]
    , a [href "/menu"] [text "go take a look at the menu!"]
    , p [] [text "or"]
    , a [href "/adminCategories"] [text "Manage categories"]
    , p [] []
    , a [href "/adminProducts"] [text "Manage products"]
    ]
