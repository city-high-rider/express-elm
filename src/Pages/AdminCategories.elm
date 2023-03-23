module Pages.AdminCategories exposing (..)

import Category exposing (Category, getCategories)
import Colorscheme
import Element exposing (Element, centerX, column, el, fill, layout, link, maximum, mouseOver, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button, option, radio)
import ErrorViewing exposing (viewHttpErrorStyled)
import Form.Category
import Html exposing (Html)
import Pages.AdminPageUtils exposing (showModelStatusStyle)
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
    layout [ Background.color Colorscheme.light.fg, Font.color Colorscheme.light.bg ] <|
        column [ width fill, centerX ]
            [ case model.availableCats of
                RemoteData.NotAsked ->
                    el [ Font.size 30 ] (text "developer forgot to send an http request")

                RemoteData.Loading ->
                    column []
                        [ el [ Font.size 30 ] (text "Getting data from the server!")
                        , text "please wait..."
                        ]

                RemoteData.Failure error ->
                    column []
                        [ el [ Font.size 30 ] (text "Unable to load data from the server!")
                        , viewHttpErrorStyled error
                        ]

                RemoteData.Success cats ->
                    case model.credentials of
                        Nothing ->
                            column [ width fill, centerX, spacing 10 ]
                                [ el [ Font.size 30, centerX, Font.color Colorscheme.light.primary ] (text "You are not logged in!")
                                , el [ centerX ] (text "You need to be logged in to change the menu.")
                                , link
                                    [ centerX
                                    , Font.color Colorscheme.light.secondary
                                    , mouseOver [ Font.color Colorscheme.light.misc ]
                                    ]
                                    { url = "/login", label = text "Log in" }
                                ]

                        Just _ ->
                            viewLoaded model cats
            ]


viewChoices : Element Msg
viewChoices =
    column [ centerX ]
        [ el [ Font.size 30, centerX, Font.color Colorscheme.light.primary ] (text "What would you like to do?")
        , row [ spacing 20 ]
            [ button [] { onPress = Just <| ChangeAction Creating, label = text "Create a category" }
            , button [] { onPress = Just <| ChangeAction Editing, label = text "Edit a category" }
            , button [] { onPress = Just <| ChangeAction <| Deleting False, label = text "Delete a category" }
            ]
        ]


showRelevantForm : Model -> List Category -> Element Msg
showRelevantForm model cats =
    case model.userAction of
        NotPicked ->
            el [ Font.size 20 ] (text "Give me something to do !")

        Creating ->
            column []
                [ el [ Font.size 20 ] (text "Create a category")
                , categoryForm model.workingCat
                ]

        Editing ->
            column []
                [ el [ Font.size 20 ] (text "Edit a category")
                , editSection model.workingCat cats
                ]

        Deleting isConfirmShowing ->
            column []
                [ el [ Font.size 20 ] (text "Remove a category")
                , deleteForm isConfirmShowing model.workingCat cats
                ]


viewLoaded : Model -> List Category -> Element Msg
viewLoaded model cats =
    column [ width <| maximum 1000 fill, centerX ]
        [ viewChoices
        , showRelevantForm model cats
        , showModelStatusStyle model.status
        ]


categoryForm : WorkingCategory -> Element Msg
categoryForm workingCat =
    let
        cat =
            case workingCat of
                NotSelected ->
                    Category.empty

                Selected c ->
                    c
    in
    column []
        [ Form.Category.categoryForm cat (ChangeWorkingCat << Selected)
        , button [] { onPress = Just (Submit Creating cat), label = text "Submit" }
        ]


deleteForm : Bool -> WorkingCategory -> List Category -> Element Msg
deleteForm confirmShowing working cats =
    column []
        [ pickCatFromIdList working cats
        , case working of
            NotSelected ->
                el [] (text "pick something to delete first!")

            Selected workingCat ->
                column []
                    [ button [] { onPress = Just (ToggleDeleteConfirm True), label = text "Delete" }
                    , confirmDelete workingCat confirmShowing
                    ]
        ]


confirmDelete : Category -> Bool -> Element Msg
confirmDelete cat isShowing =
    if not isShowing then
        Element.none

    else
        column []
            [ el [] (text "Are you sure?")
            , button [] { onPress = Just <| ToggleDeleteConfirm False, label = text "No!" }
            , button [] { onPress = Just <| Delete cat, label = text "Yes!" }
            ]


editSection : WorkingCategory -> List Category -> Element Msg
editSection editingCat cats =
    column []
        [ pickCatFromIdList editingCat cats
        , showEditFormOrNothing editingCat
        ]


pickCatFromIdList : WorkingCategory -> List Category -> Element Msg
pickCatFromIdList working cats =
    let
        sel =
            case working of
                NotSelected ->
                    Nothing

                Selected c ->
                    Just <| Category.catIdToString c.id
    in
    radio []
        { onChange = ChangeWorkingCat << getCatById cats
        , options = List.map (\c -> (option <| Category.catIdToString c.id) (text c.name)) cats
        , selected = sel
        , label = Element.Input.labelAbove [] <| text "Select category"
        }



{-
   select [ onInput (ChangeWorkingCat << getCatById cats) ]
       (option [ value "Nothing" ] [ text "select..." ] :: Form.Category.catsToOptions cats)
-}


showEditFormOrNothing : WorkingCategory -> Element Msg
showEditFormOrNothing workingCat =
    case workingCat of
        NotSelected ->
            el [ Font.size 30 ] (text "pick a category to edit!")

        Selected cat ->
            column []
                [ Form.Category.categoryForm cat (ChangeWorkingCat << Selected)
                , button [] { onPress = Just <| Submit Editing cat, label = text "Edit" }
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
    let
        pass =
            Maybe.withDefault "" model.credentials
    in
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
                        updateCat
            in
            case Category.verifyCat category of
                Err error ->
                    ( { model | status = RemoteData.succeed ( False, "Error! : " ++ error ) }, Cmd.none )

                Ok cat ->
                    ( model, request pass (RemoteData.fromResult >> ServerFeedback) cat )

        Delete cat ->
            ( model, deleteCat pass (RemoteData.fromResult >> ServerFeedback) cat.id )

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
