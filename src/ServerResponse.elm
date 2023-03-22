module ServerResponse exposing (..)

import Json.Decode as Decode exposing (bool, field, string)


type alias ServerResponse =
    ( Bool, String )


responseDecoder : Decode.Decoder ServerResponse
responseDecoder =
    Decode.map2 (\s m -> ( s, m )) (field "success" bool) (field "message" string)


succeed : String -> ServerResponse
succeed s =
    ( True, s )


responseToString : ServerResponse -> String
responseToString ( s, m ) =
    if s then
        m

    else
        "Action failed: " ++ m
