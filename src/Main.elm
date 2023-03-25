module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, a, div, h2, nav, p, text)
import Html.Attributes exposing (href)
import Pages.AdminCategories as CategoriesPageFile
import Pages.AdminProducts as ProductsPageFile
import Pages.Home as HomePageFile
import Pages.LoginPage as LoginPageFile
import Pages.Menu as MenuPageFile
import Pages.Orders as OrderPageFile
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
    , credentials : Maybe String
    }


type Page
    = NotFoundPage
    | HomePage
    | Menu MenuPageFile.Model
    | CategoriesPage CategoriesPageFile.Model
    | ProductsPage ProductsPageFile.Model
    | LoginPage LoginPageFile.Model
    | OrdersPage OrderPageFile.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            , credentials = Nothing
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

                AdminCategories ->
                    let
                        ( createModel, createCmds ) =
                            CategoriesPageFile.init model.credentials
                    in
                    ( CategoriesPage createModel, Cmd.map CatMsg createCmds )

                AdminProducts ->
                    let
                        ( prodModel, prodCmds ) =
                            ProductsPageFile.init model.credentials
                    in
                    ( ProductsPage prodModel, Cmd.map ProdMsg prodCmds )

                OrdersRoute ->
                    let
                        ( iModel, iCmds ) =
                            OrderPageFile.init model.credentials
                    in
                    ( OrdersPage iModel, Cmd.map OrderMsg iCmds )

                LoginRoute ->
                    let
                        ( iModel, iCmds ) =
                            LoginPageFile.init
                    in
                    ( LoginPage iModel, Cmd.map LoginMsg iCmds )
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

        CategoriesPage createModel ->
            Html.map CatMsg (CategoriesPageFile.view createModel)

        ProductsPage prodModel ->
            Html.map ProdMsg (ProductsPageFile.view prodModel)

        LoginPage lModel ->
            Html.map LoginMsg (LoginPageFile.view lModel)

        OrdersPage oModel ->
            Html.map OrderMsg (OrderPageFile.view oModel)


notFoundView : Html Msg
notFoundView =
    div []
        [ h2 [] [ text "the page you requested was not found." ]
        ]


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | MenuMsg MenuPageFile.Msg
    | CatMsg CategoriesPageFile.Msg
    | ProdMsg ProductsPageFile.Msg
    | LoginMsg LoginPageFile.Msg
    | OrderMsg OrderPageFile.Msg


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

        ( CatMsg cMsg, CategoriesPage cModel ) ->
            let
                ( newModel, cmds ) =
                    CategoriesPageFile.update cMsg cModel
            in
            ( { model | page = CategoriesPage newModel }
            , Cmd.map CatMsg cmds
            )

        ( ProdMsg pMsg, ProductsPage pModel ) ->
            let
                ( newModel, cmds ) =
                    ProductsPageFile.update pMsg pModel
            in
            ( { model | page = ProductsPage newModel }
            , Cmd.map ProdMsg cmds
            )

        ( LoginMsg lMsg, LoginPage lModel ) ->
            let
                ( newModel, cmds, pass ) =
                    LoginPageFile.update lMsg lModel
            in
            ( { model | page = LoginPage newModel, credentials = pass }
            , Cmd.map LoginMsg cmds
            )

        ( OrderMsg oMsg, OrdersPage oModel ) ->
            let
                ( newModel, cmds ) =
                    OrderPageFile.update oMsg oModel
            in
            ( { model | page = OrdersPage newModel }
            , Cmd.map OrderMsg cmds
            )

        ( _, _ ) ->
            ( model, Cmd.none )
