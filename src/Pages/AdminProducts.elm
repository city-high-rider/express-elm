module Pages.AdminProducts exposing (..)

import Category exposing (Category, getCategories)
import ErrorViewing exposing (viewHttpError)
import Form.Product exposing (prodsToOptions)
import Html exposing (Html, button, div, h2, h3, option, p, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Pages.AdminPageUtils exposing (createSuccessMessage)
import Products exposing (Product, UserInputProduct, getProducts, prodToString)
import RemoteData exposing (WebData)
import Requests


type alias Model =
    { workingProduct : UserInputProduct
    , availableCats : WebData (List Category)
    , availableProds : WebData (List Product)
    , action : Action
    , status : String
    }


type Action
    = Creating
    | Editing
    | Deleting Bool
    | NotPicked


init : ( Model, Cmd Msg )
init =
    ( emptyModel
    , Cmd.batch [ getCategories GotCats, getProducts GotProds ]
    )


emptyModel : Model
emptyModel =
    Model
        Products.empty
        RemoteData.Loading
        RemoteData.Loading
        NotPicked
        ""


view : Model -> Html Msg
view model =
    div []
        [ showActionButtons
        , viewMain model
        , h3 [] [ text model.status ]
        ]


showActionButtons : Html Msg
showActionButtons =
    div []
        [ button [ onClick (ChangeAction Creating) ] [ text "Create a product" ]
        , button [ onClick (ChangeAction Editing) ] [ text "Edit a product" ]
        , button [ onClick (ChangeAction (Deleting False)) ] [ text "Delete a product" ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    let
        availableStuff =
            RemoteData.map2 (\cats prods -> ( cats, prods )) model.availableCats model.availableProds
    in
    case availableStuff of
        RemoteData.NotAsked ->
            h2 [] [ text "Developer forgot to send http request" ]

        RemoteData.Loading ->
            h2 [] [ text "Loading... please wait!" ]

        RemoteData.Failure error ->
            div []
                [ h3 [] [ text "Error:" ]
                , viewHttpError error
                ]

        RemoteData.Success ( cats, prods ) ->
            showForms model cats prods


showForms : Model -> List Category -> List Product -> Html Msg
showForms model cats prods =
    case model.action of
        NotPicked ->
            h3 [] [ text "Give me something to do!" ]

        Creating ->
            createProductForm model.workingProduct cats

        Editing ->
            editingForm model.workingProduct cats prods

        Deleting _ ->
            h3 [] [ text "todo" ]


editingForm : UserInputProduct -> List Category -> List Product -> Html Msg
editingForm uInput cats prods =
    div []
        [ selectProductArea prods
        , showEditOrNothing cats uInput
        ]


selectProductArea : List Product -> Html Msg
selectProductArea prods =
    select [ onInput (UpdatedProduct << getProductById prods) ]
        (option [ value "" ] [ text "Select.." ] :: prodsToOptions prods)


showEditOrNothing : List Category -> UserInputProduct -> Html Msg
showEditOrNothing cats uInput =
    if uInput == Products.empty then
        p [] [ text "pick something to edit!" ]

    else
        div []
            [ Form.Product.productForm cats uInput UpdatedProduct
            , showProductErrorOrButton Editing uInput
            ]


getProductById : List Product -> String -> UserInputProduct
getProductById prods id =
    case List.head <| List.filter (\p -> String.fromInt p.id == id) prods of
        Nothing ->
            Products.empty

        Just p ->
            prodToString p


createProductForm : UserInputProduct -> List Category -> Html Msg
createProductForm workingProduct cats =
    div []
        [ Form.Product.productForm cats workingProduct UpdatedProduct
        , showProductErrorOrButton Creating workingProduct
        ]


showProductErrorOrButton : Action -> UserInputProduct -> Html Msg
showProductErrorOrButton action userInput =
    case Products.userInputToProduct userInput of
        Err error ->
            div []
                [ h3 [] [ text "Input error: " ]
                , p [] [ text error ]
                ]

        Ok parsedProd ->
            div []
                [ h3 [] [ text "Input ok" ]
                , button [ onClick (PostProduct action parsedProd) ] [ text "Submit" ]
                ]


type Msg
    = GotCats (WebData (List Category))
    | GotProds (WebData (List Product))
    | ChangeAction Action
    | UpdatedProduct UserInputProduct
    | PostProduct Action Product
    | ServerFeedback String (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeAction newAction ->
            ( { model | action = newAction, workingProduct = Products.empty }, Cmd.none )

        GotCats cats ->
            ( { model | availableCats = cats }, Cmd.none )

        GotProds prods ->
            ( { model | availableProds = prods }, Cmd.none )

        UpdatedProduct newProd ->
            ( { model | workingProduct = newProd }, Cmd.none )

        PostProduct Creating prod ->
            ( model, Requests.submitProduct ServerFeedback prod )

        PostProduct _ _ ->
            ( model, Cmd.none )

        ServerFeedback action result ->
            ( { emptyModel | status = createSuccessMessage result action }
            , Category.getCategories GotCats
            )
