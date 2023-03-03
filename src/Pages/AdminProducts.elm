module Pages.AdminProducts exposing (..)

import Category exposing (Category, getCategories)
import ErrorViewing exposing (viewHttpError)
import Form.Product
import Html exposing (Html, button, div, h2, h3, p, text)
import Html.Events exposing (onClick)
import Http
import Pages.AdminPageUtils exposing (createSuccessMessage)
import Products exposing (Product, UserInputProduct)
import RemoteData exposing (WebData)
import Requests


type alias Model =
    { productToSubmit : UserInputProduct
    , availableCats : WebData (List Category)
    , status : String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel
    , getCategories GotCats
    )


emptyModel : Model
emptyModel =
    Model
        Products.empty
        RemoteData.Loading
        ""


view : Model -> Html Msg
view model =
    div []
        [ showProductForm model
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
                , showProductErrorOrButton model.productToSubmit
                ]


showProductErrorOrButton : UserInputProduct -> Html Msg
showProductErrorOrButton userInput =
    case Products.userInputToProduct userInput of
        Err error ->
            div []
                [ h3 [] [ text "Input error: " ]
                , p [] [ text error ]
                ]

        Ok parsedProd ->
            div []
                [ h3 [] [ text "Input ok" ]
                , button [ onClick (PostProduct parsedProd) ] [ text "Create" ]
                ]


type Msg
    = GotCats (WebData (List Category))
    | UpdatedProduct UserInputProduct
    | PostProduct Product
    | ServerFeedback String (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCats cats ->
            ( { model | availableCats = cats }, Cmd.none )

        UpdatedProduct newProd ->
            ( { model | productToSubmit = newProd }, Cmd.none )

        PostProduct prod ->
            ( model, Requests.submitProduct ServerFeedback prod )

        ServerFeedback action result ->
            ( { emptyModel | status = createSuccessMessage result action }
            , Category.getCategories GotCats
            )
