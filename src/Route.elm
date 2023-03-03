module Route exposing (..)

import Category exposing (CategoryId, categoryParser)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | MenuRoute 
    | AdminCategories
    | AdminProducts


parseUrl : Url -> Route
parseUrl url =
    parse routeParser url
        |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map MenuRoute (s "menu")
        , map AdminCategories (s "adminCategories")
        , map AdminProducts (s "adminProducts")
        ]
