module Pages.AdminPageUtils exposing (..)

import Element exposing (Element, el)
import ErrorViewing exposing (httpErrorToString)
import Html exposing (Html, div, p, text)
import RemoteData exposing (WebData)
import ServerResponse exposing (ServerResponse, responseToString)


createSuccessMessage : Result e a -> String -> String
createSuccessMessage result action =
    case result of
        Err _ ->
            "Something went wrong!"

        Ok _ ->
            "successfully " ++ action


showModelStatus : WebData ServerResponse -> Html msg
showModelStatus webR =
    case webR of
        RemoteData.NotAsked ->
            p [] [ text "You haven't done anything yet" ]

        RemoteData.Loading ->
            p [] [ text "Loading... please wait" ]

        RemoteData.Failure err ->
            p [] [ text <| "Failed to reach the server : " ++ httpErrorToString err ]

        RemoteData.Success r ->
            p [] [ text <| responseToString r ]


showModelStatusStyle : WebData ServerResponse -> Element msg
showModelStatusStyle webR =
    case webR of
        RemoteData.NotAsked ->
            el [] (Element.text "You haven't done anything yet")

        RemoteData.Loading ->
            el [] (Element.text "Loading... please wait")

        RemoteData.Failure err ->
            el [] (Element.text <| "Failed to reach the server : " ++ httpErrorToString err)

        RemoteData.Success r ->
            el [] (Element.text <| responseToString r)
