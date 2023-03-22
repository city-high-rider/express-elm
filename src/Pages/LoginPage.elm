module Pages.LoginPage exposing (..)

import Colorscheme exposing (Colorscheme)
import Element exposing (alignRight, centerX, column, el, fill, layout, link, maximum, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, currentPassword)
import Html exposing (Html)
import Pages.AdminPageUtils exposing (showModelStatusStyle)
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
    layout [ Background.color Colorscheme.light.fg ] <|
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
            , button [ alignRight ] { onPress = Just Submit, label = buttonLabel "submit" }
            , button [ alignRight ] { onPress = Just Logout, label = buttonLabel "logout" }
            , showModelStatusStyle model.status
            , row [ spacing 10 ]
                [ link
                    [ Font.color Colorscheme.light.secondary
                    , Element.mouseOver [ Font.color Colorscheme.light.misc ]
                    ]
                    { url = "/adminCategories", label = text "Back to categories" }
                , link
                    [ Font.color Colorscheme.light.secondary
                    , Element.mouseOver [ Font.color Colorscheme.light.misc ]
                    ]
                    { url = "/adminProducts", label = text "Back to products" }
                ]
            ]


buttonLabel : String -> Element.Element msg
buttonLabel txt =
    el
        [ Background.color Colorscheme.light.bg
        , padding 10
        , Font.color Colorscheme.light.primary
        , Border.rounded 8
        ]
        (text txt)


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
