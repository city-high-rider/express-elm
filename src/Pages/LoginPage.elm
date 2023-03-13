module Pages.LoginPage exposing (..)

import Html exposing (Html, br, button, div, h3, input, p, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Pages.AdminPageUtils exposing (showModelStatus)
import RemoteData exposing (WebData)
import Requests exposing (checkPassword)
import ServerResponse exposing (ServerResponse)


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
        , button [ onClick Logout ] [ text "Log out" ]
        , br [] []
        , showModelStatus model.status
        ]


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
            ( model, checkPassword (RemoteData.fromResult >> Reply) model.input, Nothing )

        Reply w ->
            let
                toSubmit =
                    case w of
                        RemoteData.Success ( True, _ ) ->
                            Just model.input

                        _ ->
                            Nothing
            in
            ( { model | status = w }, Cmd.none, toSubmit )

        Logout ->
            ( model, Cmd.none, Nothing )
