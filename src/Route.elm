module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | MenuRoute
    | AdminCategories
    | AdminProducts
    | LoginRoute


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
        , map LoginRoute (s "login")
        ]
