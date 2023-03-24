module ErrorViewing exposing (httpErrorToString, viewHttpError, viewHttpErrorStyled)

import Element exposing (Element)
import Html exposing (Html, p, text)
import Http


viewHttpError : Http.Error -> Html msg
viewHttpError err =
    p [] [ text <| httpErrorToString err ]


viewHttpErrorStyled : Http.Error -> Element msg
viewHttpErrorStyled err =
    Element.text <| httpErrorToString err


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl s ->
            s

        Http.Timeout ->
            "Time out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus s ->
            "Bad status :" ++ String.fromInt s

        Http.BadBody s ->
            "Bad body :" ++ s
