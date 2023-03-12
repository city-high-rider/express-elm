module Pages.LoginPage exposing (..)

import Html exposing (Html, div, input, p, text, button, h3, br)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Requests exposing (checkPassword)
import Http


type alias Model =
    { input : String
    , status : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" ""
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
        , p [] [ text model.status ]
        ]


type Msg
    = ChangedInput String
    | Submit
    | Logout
    | ServerResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        ChangedInput newInput ->
            ( { model | input = newInput }, Cmd.none, Nothing )

        Submit ->
            ( model, checkPassword (ServerResponse) model.input, Nothing )

        ServerResponse (Err _) ->
            ( { model | status = "Login failed!" }, Cmd.none, Nothing )

        ServerResponse (Ok r) ->
            ( { model | status = "Server response : " ++ r }, Cmd.none, Just model.input )

        Logout ->
            ( {model | status = "Logged out"}, Cmd.none, Nothing)
