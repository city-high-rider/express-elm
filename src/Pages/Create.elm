module Pages.Create exposing (..)

import Category exposing (Category, catIdToString, getCategories)
import ErrorViewing exposing (viewHttpError)
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import RemoteData exposing (WebData)


type alias Model =
    { catToSubmit : Category
    , catToDelete : Maybe Category.CategoryId
    , availableCats : WebData (List Category)
    , successStatus : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Category.empty Nothing RemoteData.Loading "Waiting for input...", getCategories GotCats )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Create a category" ]
        , viewForm
        , h2 [] [ text "Remove a category" ]
        , deleteForm model
        , p [] [ text model.successStatus ]
        , showSelected model.catToDelete
        ]


showSelected : Maybe Category.CategoryId -> Html Msg
showSelected maybe =
    div []
        [ text (Maybe.withDefault "Nothing selected!" (Maybe.map catIdToString maybe))
        ]


viewForm : Html Msg
viewForm =
    Html.form []
        [ div []
            [ text "Category name"
            , br [] []
            , input [ type_ "text", onInput StoreName ] []
            ]
        , br [] []
        , div []
            [ text "Units of size measurement"
            , br [] []
            , input [ type_ "text", onInput StoreUnits ] []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick Submit ]
                [ text "Create" ]
            ]
        ]


deleteForm : Model -> Html Msg
deleteForm model =
    case model.availableCats of
        RemoteData.NotAsked ->
            p [] [ text "You never asked for the category data!" ]

        RemoteData.Loading ->
            p [] [ text "Getting categories from server.. please wait" ]

        RemoteData.Failure err ->
            div []
                [ h3 [] [ text "Couldn't get categories!" ]
                , viewHttpError err
                ]

        RemoteData.Success cats ->
            Html.form []
                [ div []
                    [ select [ onInput ClickedCat ] (defaultOption :: catsToOptions cats)
                    ]
                , div []
                    [ button [ type_ "button", onClick (Delete model.catToDelete) ]
                        [ text "Delete" ]
                    ]
                ]


defaultOption : Html Msg
defaultOption =
    option [ value "Nothing" ] [ text "Select..." ]


catsToOptions : List Category -> List (Html Msg)
catsToOptions cats =
    List.map catToOption cats


catToOption : Category -> Html Msg
catToOption cat =
    option [ value (String.fromInt <| Category.catIdToInt cat.id) ] [ text cat.name ]


type Msg
    = StoreName String
    | StoreUnits String
    | Submit
    | Delete (Maybe Category.CategoryId)
    | CatDeleted (Result Http.Error String)
    | CatCreated (Result Http.Error Category)
    | ClickedCat String
    | GotCats (WebData (List Category))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreName val ->
            let
                oldCat =
                    model.catToSubmit

                newCat =
                    { oldCat | name = val }
            in
            ( { model | catToSubmit = newCat }, Cmd.none )

        StoreUnits val ->
            let
                oldCat =
                    model.catToSubmit

                newCat =
                    { oldCat | units = val }
            in
            ( { model | catToSubmit = newCat }, Cmd.none )

        ClickedCat str ->
            ( { model | catToDelete = Maybe.map Category.intToCatId (String.toInt str) }, Cmd.none )

        Submit ->
            ( model, submitResult model.catToSubmit )

        Delete Nothing ->
            ( { model | successStatus = "Can't delete nothing!" }, Cmd.none )

        Delete (Just id) ->
            ( model, deleteCat id )

        CatCreated (Ok _) ->
            ( { model | successStatus = "Created the post successfully!" }, Cmd.none )

        CatCreated (Err _) ->
            ( { model | successStatus = "There was an issue creating the category!" }, Cmd.none )

        CatDeleted (Err _) ->
            ( { model | successStatus = "There was an issue deleting the category!" }, Cmd.none )

        CatDeleted (Ok _) ->
            ( { model | successStatus = "Successfully deleted the category" }, Cmd.none )

        GotCats cats ->
            ( { model | availableCats = cats }, Cmd.none )


submitResult : Category -> Cmd Msg
submitResult cat =
    Http.post
        { url = "http://localhost:3000/newCat"
        , body = Http.jsonBody (Category.newCatEncoder cat)

        -- this won't do anything because our server doesn't send json back yet
        , expect = Http.expectJson CatCreated Category.catDecoder
        }


deleteCat : Category.CategoryId -> Cmd Msg
deleteCat id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/deleteCat/" ++ catIdToString id
        , body = Http.emptyBody
        , expect = Http.expectString CatDeleted
        , timeout = Nothing
        , tracker = Nothing
        }
