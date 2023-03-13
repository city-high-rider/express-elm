module Pages.AdminCategories exposing (..)

import Category exposing (Category, getCategories)
import ErrorViewing exposing (viewHttpError)
import Form.Category
import Html exposing (..)
import Html.Attributes exposing (href, type_, value)
import Html.Events exposing (onClick, onInput)
import Pages.AdminPageUtils exposing (showModelStatus)
import RemoteData exposing (WebData)
import Requests exposing (deleteCat, submitResult, updateCat)
import ServerResponse exposing (ServerResponse)


type alias Model =
    { availableCats : WebData (List Category)
    , workingCat : WorkingCategory
    , userAction : Action
    , status : WebData ServerResponse
    , credentials : Maybe String
    }


type WorkingCategory
    = NotSelected
    | Selected Category


type Action
    = Editing
    | Creating
    | Deleting Bool
    | NotPicked


emptyModel : Model
emptyModel =
    Model
        RemoteData.Loading
        NotSelected
        NotPicked
        RemoteData.NotAsked
        Nothing


init : Maybe String -> ( Model, Cmd Msg )
init credentials =
    ( { emptyModel | credentials = credentials }, getCategories GotCats )


view : Model -> Html Msg
view model =
    case model.availableCats of
        RemoteData.NotAsked ->
            div []
                [ h3 [] [ text "developer forgot to send an http request" ]
                ]

        RemoteData.Loading ->
            div []
                [ h3 [] [ text "Getting data from the server!" ]
                , p [] [ text "please wait..." ]
                ]

        RemoteData.Failure error ->
            div []
                [ h3 [] [ text "Unable to load data from the server!" ]
                , viewHttpError error
                ]

        RemoteData.Success cats ->
            case model.credentials of
                Nothing ->
                    div []
                        [ h3 [] [ text "You are not logged in!" ]
                        , p [] [ text "You need to be logged in to change the menu." ]
                        , a [ href "/login" ] [ text "Login page" ]
                        ]

                Just _ ->
                    viewLoaded model cats


viewChoices : Html Msg
viewChoices =
    div []
        [ h2 [] [ text "What would you like to do?" ]
        , button [ onClick (ChangeAction Creating) ] [ text "Create a category" ]
        , button [ onClick (ChangeAction Editing) ] [ text "Edit a category" ]
        , button [ onClick (ChangeAction (Deleting False)) ] [ text "Remove a category" ]
        ]


showRelevantForm : Model -> List Category -> Html Msg
showRelevantForm model cats =
    case model.userAction of
        NotPicked ->
            p [] [ text "Give me something to do !" ]

        Creating ->
            div []
                [ h2 [] [ text "Create a category" ]
                , categoryForm model.workingCat
                ]

        Editing ->
            div []
                [ h2 [] [ text "Edit a category" ]
                , editSection model.workingCat cats
                ]

        Deleting isConfirmShowing ->
            div []
                [ h2 [] [ text "Remove a category" ]
                , deleteForm cats
                , confirmDelete model.workingCat isConfirmShowing
                ]


viewLoaded : Model -> List Category -> Html Msg
viewLoaded model cats =
    div []
        [ viewChoices
        , showRelevantForm model cats
        , showModelStatus model.status
        ]


categoryForm : WorkingCategory -> Html Msg
categoryForm workingCat =
    let
        cat =
            case workingCat of
                NotSelected ->
                    Category.empty

                Selected c ->
                    c
    in
    Html.form []
        [ Form.Category.categoryForm cat (ChangeWorkingCat << Selected)
        , div []
            [ button [ type_ "button", onClick (Submit Creating cat) ]
                [ text "Create" ]
            ]
        ]


deleteForm : List Category -> Html Msg
deleteForm cats =
    Html.form []
        [ pickCatFromIdList cats
        , div []
            [ button [ type_ "button", onClick (ToggleDeleteConfirm True) ]
                [ text "Delete" ]
            ]
        ]


confirmDelete : WorkingCategory -> Bool -> Html Msg
confirmDelete workingCat isShowing =
    if not isShowing then
        div [] []

    else
        case workingCat of
            NotSelected ->
                h3 [] [ text "pick something to delete first!" ]

            Selected cat ->
                div []
                    [ p [] [ text "Are you sure?" ]
                    , button [ onClick (ToggleDeleteConfirm False) ] [ text "No!" ]
                    , button [ onClick (Delete cat) ] [ text "Yes!" ]
                    ]


editSection : WorkingCategory -> List Category -> Html Msg
editSection editingCat cats =
    div []
        [ pickCatFromIdList cats
        , showEditFormOrNothing editingCat
        ]


pickCatFromIdList : List Category -> Html Msg
pickCatFromIdList cats =
    select [ onInput (ChangeWorkingCat << getCatById cats) ]
        (option [ value "Nothing" ] [ text "select..." ] :: Form.Category.catsToOptions cats)


showEditFormOrNothing : WorkingCategory -> Html Msg
showEditFormOrNothing workingCat =
    case workingCat of
        NotSelected ->
            h3 [] [ text "pick a category to edit!" ]

        Selected cat ->
            div []
                [ Form.Category.categoryForm cat (ChangeWorkingCat << Selected)
                , button [ onClick (Submit Editing cat) ] [ text "Edit" ]
                ]


type Msg
    = ChangeWorkingCat WorkingCategory
    | Submit Action Category
    | Delete Category
    | ServerFeedback (WebData ServerResponse)
    | GotCats (WebData (List Category))
    | ChangeAction Action
    | ToggleDeleteConfirm Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeWorkingCat newcat ->
            ( { model | workingCat = newcat }, Cmd.none )

        ChangeAction newAction ->
            ( { model | userAction = newAction, workingCat = NotSelected }, Cmd.none )

        Submit action category ->
            let
                request =
                    if action == Creating then
                        submitResult

                    else
                        updateCat (Maybe.withDefault "" model.credentials)
            in
            case Category.verifyCat category of
                Err error ->
                    ( { model | status = RemoteData.succeed ( False, "Error! : " ++ error ) }, Cmd.none )

                Ok cat ->
                    ( model, request (RemoteData.fromResult >> ServerFeedback) cat )

        Delete cat ->
            ( model, deleteCat (RemoteData.fromResult >> ServerFeedback) cat.id )

        ServerFeedback feedback ->
            ( { emptyModel
                | status = feedback
                , credentials = model.credentials
              }
            , getCategories GotCats
            )

        GotCats cats ->
            ( { model | availableCats = cats }, Cmd.none )

        ToggleDeleteConfirm showing ->
            ( { model | userAction = Deleting showing }, Cmd.none )


getCatById : List Category -> String -> WorkingCategory
getCatById cats idString =
    List.filter (\c -> Category.catIdToString c.id == idString) cats
        |> List.head
        |> Maybe.map Selected
        |> Maybe.withDefault NotSelected
