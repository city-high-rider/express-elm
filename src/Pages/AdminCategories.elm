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
import Requests exposing (deleteCat, submitResult)


type alias Model =
    { catToSubmit : Category
    , catToDelete : Maybe Category.CategoryId
    , availableCats : WebData (List Category)
    , successStatus : String
    , isConfirmShowing : Bool
    }


emptyModel : Model
emptyModel =
    Model
        Category.empty
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
        , viewForm model.catToSubmit
        , h2 [] [ text "Remove a category" ]
        , div []
            [ deleteForm model
            , confirmDelete model.catToDelete model.isConfirmShowing
            ]
        , p [] [ text model.successStatus ]
        ]


viewForm : Category -> Html Msg
viewForm toSubmit =
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


type Msg
    = UpdatedCategory Category
    | Submit
    | ToggleConfirm Bool
    | Delete (Maybe Category.CategoryId)
    | ServerFeedback String (Result Http.Error String)
    | ClickedCat String
    | GotCats (WebData (List Category))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedCategory newCat ->
            ( { model | catToSubmit = newCat }, Cmd.none )

        ClickedCat str ->
            ( { model | catToDelete = Maybe.map Category.intToCatId (String.toInt str) }, Cmd.none )

        Submit ->
            case Category.verifyCat model.catToSubmit of
                Err error ->
                    ( { model | successStatus = "Error creating category: " ++ error }
                    , Cmd.none
                    )

                Ok cat ->
                    ( model, submitResult ServerFeedback cat )

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
