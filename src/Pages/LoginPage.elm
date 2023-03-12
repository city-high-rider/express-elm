module Pages.LoginPage exposing (..)

import Html exposing (Html, div, input, p, text, button, h3, br)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Requests exposing (checkPassword)
import Http
import RemoteData exposing (WebData)
import ServerResponse exposing (ServerResponse, responseToString)
import ErrorViewing exposing (httpErrorToString)


type alias Model =
    { input : String
    , status : WebData ServerResponse
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" RemoteData.NotAsked
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Enter the admin password" ]
        , input [ onInput ChangedInput, type_ "text", value model.input ] []
        , button [ onClick Submit ] [ text "Enter" ]
        , br [] []
        , button [onClick Logout] [text "Log out"]
        , br [] []
        , showModelStatus model.status
        ]


showModelStatus : WebData ServerResponse -> Html Msg
showModelStatus webR =
    case webR of
        RemoteData.NotAsked ->
            p [] [text "You haven't done anything yet"]

        RemoteData.Loading ->
            p [] [text "Loading... please wait"]

        RemoteData.Failure err ->
            p [] [text "Failed to reach the server : " ++ httpErrorToString err]

        RemoteData.Success r ->
            p [] [text <| responseToString r]

    

type Msg
    = ChangedInput String
    | Submit
    | Logout
    | Reply (WebData ServerResponse)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        ChangedInput newInput ->
            ( { model | input = newInput }, Cmd.none, Nothing )

        Submit ->
            ( model, checkPassword ( RemoteData.fromResult >> Reply ) model.input, Nothing )

        Reply w ->
            ( { model | status = w}, Cmd.none, Nothing )

        Logout ->
            ( model, Cmd.none, Nothing)
