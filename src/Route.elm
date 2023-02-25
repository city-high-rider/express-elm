module Route exposing (..)
import Url exposing (Url)
import Url.Parser exposing (..)

type Route
    = NotFound
    | Home
    | Menu


parseUrl : Url -> Route
parseUrl url =
    parse routeParser url
    |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf 
    [ map Home top
    , map Menu (s "menu")
    ]
