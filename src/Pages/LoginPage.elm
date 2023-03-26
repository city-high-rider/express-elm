module Pages.LoginPage exposing (..)

import Colorscheme
import Element exposing (alignRight, centerX, column, fill, layout, link, maximum, padding, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button, currentPassword)
import Html exposing (Html)
import Pages.AdminPageUtils exposing (showModelStatusStyle)
import RemoteData exposing (WebData)
import Requests exposing (checkPassword)
import ServerResponse exposing (ServerResponse)
import StyleLabels exposing (buttonLabel, layoutWithHeader, linkLabel)


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
    layoutWithHeader [ Background.color Colorscheme.light.fg ] <|
        column
            [ spacing 10
            , padding 10
            , Font.color Colorscheme.light.bg
            , width <| maximum 1000 fill
            , centerX
            ]
            [ currentPassword []
                { onChange = ChangedInput
                , text = model.input
                , placeholder = Nothing
                , label =
                    Element.Input.labelAbove
                        [ Font.color Colorscheme.light.primary
                        , Font.size 30
                        , centerX
                        ]
                    <|
                        text "Enter the admin password"
                , show = True
                }
            , button [ alignRight ] { onPress = Just Submit, label = buttonLabel "submit" [] }
            , button [ alignRight ] { onPress = Just Logout, label = buttonLabel "logout" [] }
            , showModelStatusStyle model.status
            , row [ spaceEvenly, width fill ]
                [ link [] { url = "/adminCategories", label = linkLabel "Back to categories" [] }
                , link [] { url = "/adminProducts", label = linkLabel "Back to products" [] }
                , link [] { url = "/", label = linkLabel "Back to home" [] }
                , link [] { url = "/orders", label = linkLabel "Back to orders" [] }
                ]
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
            ( { model | status = RemoteData.succeed <| ServerResponse.succeed "Logged out" }, Cmd.none, Nothing )
