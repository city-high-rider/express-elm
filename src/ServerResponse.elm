module ServerResponse exposing (..)

import Json.Decode as Decode exposing (string, field)


type ServerResponse
    = Failure String
    | Success String


responseDecoder : Decode.Decoder ServerResponse
responseDecoder =
    Decode.map2 stringsToResponse (field "state" string) (field "messgage" string)


stringsToResponse : String -> String -> ServerResponse
stringsToResponse state message =
    if state == "Success" then
        Success message
    else 
        Failure message
