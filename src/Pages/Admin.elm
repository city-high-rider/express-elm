module Pages.Admin exposing (..)

import Category exposing (Category, catIdToString, getCategories)
import ErrorViewing exposing (viewHttpError)
import Form.Category
import Form.Product
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Products exposing (Product)
import RemoteData exposing (WebData)


type alias Model =
    { catToSubmit : Category
    , productToSubmit : Product
    , catToDelete : Maybe Category.CategoryId
    , availableCats : WebData (List Category)
    , successStatus : String
    , isConfirmShowing : Bool
    }


emptyModel : Model
emptyModel =
    Model
        Category.empty
        Products.empty
        Nothing
        RemoteData.Loading
        "Waiting for input..."
        False


init : ( Model, Cmd Msg )
init =
    ( emptyModel, getCategories GotCats )


refreshModel : String -> Cmd Msg -> ( Model, Cmd Msg )
refreshModel resultMsg otherCmds =
    let
        ( initialModel, initialCmds ) =
            init
    in
    ( { initialModel | successStatus = resultMsg }
    , Cmd.batch [ otherCmds, initialCmds ]
    )


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
        , div []
            [ h2 [] [text "add a product"]
            , showProductForm model
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


showProductForm : Model -> Html Msg
showProductForm model =
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
                [ Form.Product.productForm cats model.productToSubmit UpdatedProduct
                ]


type Msg
    = UpdatedCategory Category
    | UpdatedProduct (Maybe Product)
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

        UpdatedProduct Nothing ->
            ( { model | successStatus = "Invalid input for product form!" }, Cmd.none )

        UpdatedProduct (Just newProd) ->
            ( { model | productToSubmit = newProd }, Cmd.none )

        ClickedCat str ->
            ( { model | catToDelete = Maybe.map Category.intToCatId (String.toInt str) }, Cmd.none )

        Submit ->
            case Category.verifyCat model.catToSubmit of
                Err error ->
                    ( { model | successStatus = "Error creating category: " ++ error }
                    , Cmd.none
                    )

                Ok cat ->
                    ( model, submitResult cat )

        Delete Nothing ->
            ( { model | successStatus = "Can't delete nothing!" }, Cmd.none )

        Delete (Just id) ->
            ( model, deleteCat id )

        ServerFeedback action result ->
            refreshModel (createSuccessMessage result action) Cmd.none

        GotCats cats ->
            ( { model | availableCats = cats }, Cmd.none )

        ToggleConfirm s ->
            ( { model | isConfirmShowing = s }, Cmd.none )


createSuccessMessage : Result e a -> String -> String
createSuccessMessage result action =
    case result of
        Err _ ->
            "Something went wrong!"

        Ok _ ->
            "successfully " ++ action ++ " category"


submitResult : Category -> Cmd Msg
submitResult cat =
    Http.post
        { url = "http://localhost:3000/newCat"
        , body = Http.jsonBody (Category.newCatEncoder cat)
        , expect = Http.expectString (ServerFeedback "created")
        }


deleteCat : Category.CategoryId -> Cmd Msg
deleteCat id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/deleteCat/" ++ catIdToString id
        , body = Http.emptyBody
        , expect = Http.expectString (ServerFeedback "deleted")
        , timeout = Nothing
        , tracker = Nothing
        }
