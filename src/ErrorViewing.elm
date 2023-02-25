module ErrorViewing exposing (viewHttpError)
import Http
import Html exposing (Html, p, text)

viewHttpError : Http.Error -> Html msg
viewHttpError err =
    case err of 
        Http.BadUrl s ->
            p [] [text s]

        Http.Timeout ->
            p [] [text "time out"]

        Http.NetworkError ->
            p [] [text "network error"]

        Http.BadStatus s ->
            p [] [text (String.fromInt s)]

        Http.BadBody s ->
            p [] [text s]
