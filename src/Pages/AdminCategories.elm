module Pages.AdminCategories exposing (..)

import Category exposing (Category, getCategories)
import ErrorViewing exposing (viewHttpError)
import Form.Category
import Form.Product
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import Http
import Pages.AdminPageUtils exposing (createSuccessMessage)
import RemoteData exposing (WebData)
import Requests exposing (deleteCat, submitResult, updateCat)


type alias Model =
    { catToSubmit : Category
    , catToDelete : Maybe Category.CategoryId
    , editingCat : Maybe Category
    , availableCats : WebData (List Category)
    , successStatus : String
    , isConfirmShowing : Bool
    }


emptyModel : Model
emptyModel =
    Model
        Category.empty
        Nothing
        Nothing
        RemoteData.Loading
        "Waiting for input..."
        False


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getCategories GotCats )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Create a category" ]
        , categoryForm model.catToSubmit
        , h2 [] [ text "Edit a category" ]
        , editSection model
        , h2 [] [ text "Remove a category" ]
        , div []
            [ deleteForm model
            , confirmDelete model.catToDelete model.isConfirmShowing
            ]
        , p [] [ text model.successStatus ]
        ]


categoryForm : Category -> Html Msg
categoryForm toSubmit =
    Html.form []
        [ Form.Category.categoryForm toSubmit UpdatedCategory
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
                    [ select [ onInput ClickedCat ] (Form.Product.defaultOption :: Form.Category.catsToOptions cats)
                    ]
                , div []
                    [ button [ type_ "button", onClick (ToggleConfirm True) ]
                        [ text "Delete" ]
                    ]
                ]


confirmDelete : Maybe Category.CategoryId -> Bool -> Html Msg
confirmDelete toDelete isShowing =
    if not isShowing then
        div [] []

    else
        div []
            [ p [] [ text "Are you sure?" ]
            , button [ onClick (ToggleConfirm False) ] [ text "No!" ]
            , button [ onClick (Delete toDelete) ] [ text "Yes!" ]
            ]


editSection : Model -> Html Msg
editSection model =
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
            div []
                [ pickCatToEdit cats
                , showEditFormOrNothing model.editingCat
                ]


pickCatToEdit : List Category -> Html Msg
pickCatToEdit cats =
    select [ onInput SelectedToEdit ] (Form.Category.catsToOptions cats)


showEditFormOrNothing : Maybe Category -> Html Msg
showEditFormOrNothing mCat =
    case mCat of
        Nothing ->
            p [] [ text "Pick a category to edit" ]

        Just cat ->
            div []
                [ Form.Category.categoryForm cat UpdatedEditCat
                , button [ onClick SubmitEdit ] [ text "Edit" ]
                ]


type Msg
    = UpdatedCategory Category
    | UpdatedEditCat Category
    | Submit
    | SubmitEdit
    | ToggleConfirm Bool
    | Delete (Maybe Category.CategoryId)
    | ServerFeedback String (Result Http.Error String)
    | ClickedCat String
    | SelectedToEdit String
    | GotCats (WebData (List Category))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedCategory newcat ->
            ( { model | catToSubmit = newcat }, Cmd.none )

        UpdatedEditCat newcat ->
            ( { model | editingCat = Just newcat }, Cmd.none )

        ClickedCat str ->
            ( { model | catToDelete = Maybe.map Category.intToCatId (String.toInt str) }, Cmd.none )

        SelectedToEdit str ->
            ( { model | editingCat = getCatById str model.availableCats }, Cmd.none )

        Submit ->
            case Category.verifyCat model.catToSubmit of
                Err error ->
                    ( { model | successStatus = "Error creating category: " ++ error }
                    , Cmd.none
                    )

                Ok cat ->
                    ( model, submitResult ServerFeedback cat )

        SubmitEdit ->
            case model.editingCat of
                Nothing ->
                    ( { model | successStatus = "Error editing category: Wrong one selected" }
                    , Cmd.none
                    )

                Just c ->
                    case Category.verifyCat c of
                        Err error ->
                            ( { model | successStatus = "Error creating category: " ++ error }
                            , Cmd.none
                            )

                        Ok cat ->
                            ( model, updateCat ServerFeedback cat )

        Delete Nothing ->
            ( { model | successStatus = "Can't delete nothing!" }, Cmd.none )

        Delete (Just id) ->
            ( model, deleteCat ServerFeedback id )

        ServerFeedback action result ->
            ( { emptyModel | successStatus = createSuccessMessage result action }
            , getCategories GotCats
            )

        GotCats cats ->
            ( { model | availableCats = cats }, Cmd.none )

        ToggleConfirm s ->
            ( { model | isConfirmShowing = s }, Cmd.none )


getCatById : String -> WebData (List Category) -> Maybe Category
getCatById idString cats =
    case cats of
        RemoteData.Success cs ->
            List.head <| List.filter (\c -> Category.catIdToString c.id == idString) cs

        _ ->
            Nothing
