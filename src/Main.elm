module Main exposing (..)

import Html exposing (text, div, h3, p, Html)
import Browser exposing (element)
import Http

main =
    element 
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type Model
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success String


init : () -> (Model, Cmd Msg)
init _ =
    (NotAsked, getData)


view : Model -> Html Msg
view model =
    case model of
        NotAsked ->
            p [] [text "You haven't asked for data yet."]
        
        Loading ->
            p [] [text "loading.. please wait"]

        Failure err ->
            div []
            [ h3 [] [text "something went wrong"]
            , p [] [text <| httpErrorToString err]
            ]

        Success data ->
            div []
            [ h3 [] [text "data retreived successfully"]
            , p [] [text data]
            ]


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl s ->
            s
        Http.Timeout ->
            "connection timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadBody s ->
            s

        Http.BadStatus status ->
            String.fromInt status


type Msg =
    GotData (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
    case msg of
        GotData ( Err e ) ->
            (Failure e, Cmd.none)
        GotData (Ok t ) ->
            (Success t, Cmd.none)


getData : Cmd Msg
getData =
    Http.get
    { url = "http://localhost:3000"
    , expect = Http.expectString GotData
    }
