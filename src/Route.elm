module Route exposing (..)
import Url exposing (Url)
import Url.Parser exposing (..)
import Pages.Menu exposing (CategoryId, menuRouteParser)

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
    , map MenuRoute (s "menu" </> menuRouteParser)
    ]
