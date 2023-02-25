module Route exposing (..)

import Category exposing (CategoryId, categoryParser)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | MenuRoute CategoryId
    | MenuList


parseUrl : Url -> Route
parseUrl url =
    parse routeParser url
        |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map MenuList (s "menu")
        , map MenuRoute (s "menu" </> categoryParser)
        ]
