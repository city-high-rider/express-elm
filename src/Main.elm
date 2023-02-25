module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, h2, p, text)
import Pages.Home as HomePageFile
import Pages.Menu as MenuPageFile
import Route exposing (Route(..))
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | HomePage
    | Menu MenuPageFile.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, initialCmds ) =
    let
        ( currentPage, mappedCmds ) =
            case model.route of
                NotFound ->
                    ( NotFoundPage, Cmd.none )

                Home ->
                    ( HomePage, Cmd.none )

                MenuRoute ->
                    let
                        ( menuModel, menuCmds ) =
                            MenuPageFile.init
                    in
                    ( Menu menuModel, Cmd.map MenuMsg menuCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ initialCmds, mappedCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "Elm app"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        HomePage ->
            HomePageFile.view

        Menu menuModel ->
            Html.map MenuMsg (MenuPageFile.view menuModel)


notFoundView : Html Msg
notFoundView =
    div []
        [ h2 [] [ text "the page you requested was not found." ]
        ]


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | MenuMsg MenuPageFile.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none ) |> initCurrentPage

        ( MenuMsg mMsg, Menu mModel ) ->
            let
                ( newModel, cmds ) =
                    MenuPageFile.update mMsg mModel
            in
            ( { model | page = Menu newModel }
            , Cmd.map MenuMsg cmds
            )

        ( _, _ ) ->
            ( model, Cmd.none )
