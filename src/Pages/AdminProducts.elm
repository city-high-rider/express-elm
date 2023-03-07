module Pages.AdminProducts exposing (..)

import Category exposing (Category, getCategories)
import ErrorViewing exposing (..)
import Form.Product
import Html exposing (Html, button, div, h3, option, p, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import Http
import Products exposing (Product, UserInputProduct, getProducts, prodToString)
import RemoteData exposing (WebData)
import Requests


type alias Model =
    { action : Action
    , availableCats : WebData (List Category)
    , availableProds : WebData (List Product)
    , status : String
    }


type Action
    = NotPicked
    | Creating UserInputProduct
    | Editing (Maybe UserInputWithId)
    | Deleting Bool (Maybe Int)


type alias UserInputWithId =
    ( UserInputProduct, Int )


init : ( Model, Cmd Msg )
init =
    ( emptyModel
    , Cmd.batch [ getCategories GotCats, getProducts GotProds ]
    )


emptyModel : Model
emptyModel =
    Model
        NotPicked
        RemoteData.Loading
        RemoteData.Loading
        ""


view : Model -> Html Msg
view model =
    let
        serverResources =
            RemoteData.map2 (\c p -> ( c, p )) model.availableCats model.availableProds
    in
    case serverResources of
        RemoteData.NotAsked ->
            h3 [] [ text "The developer forgot to request resources from server!" ]

        RemoteData.Loading ->
            h3 [] [ text "Getting data from the server... please wait!" ]

        RemoteData.Failure err ->
            div []
                [ h3 [] [ text "Unable to get data from the server!" ]
                , viewHttpError err
                ]

        RemoteData.Success ( cats, prods ) ->
            div []
                [ showButtons
                , showRelevantForm model.action cats prods
                , p [] [ text model.status ]
                ]


showButtons : Html Msg
showButtons =
    div []
        [ button [ onClick <| NewAction <| Creating Products.empty ] [ text "Create new product" ]
        , button [ onClick <| NewAction <| Editing Nothing ] [ text "Edit a product" ]
        , button [ onClick <| NewAction <| Deleting False Nothing ] [ text "Remove a product" ]
        ]


showRelevantForm : Action -> List Category -> List Product -> Html Msg
showRelevantForm action cats prods =
    case action of
        NotPicked ->
            h3 [] [ text "Give me something to do!" ]

        Creating uInput ->
            creatingForm uInput cats

        Editing maybeStuff ->
            viewEditStuff maybeStuff cats prods

        Deleting isConfirmShowing maybeId ->
            viewDeleteStuff isConfirmShowing maybeId prods


creatingForm : UserInputProduct -> List Category -> Html Msg
creatingForm uInput cats =
    div []
        [ h3 [] [ text "Create a product" ]
        , Form.Product.productForm cats uInput UpdateUserInput
        , submitButton uInput CreateProduct
        ]


submitButton : UserInputProduct -> (Product -> msg) -> Html msg
submitButton userInput msg =
    case Products.userInputToProduct userInput of
        Err e ->
            h3 [] [ text <| "Error: " ++ e ]

        Ok p ->
            button [ onClick (msg p) ] [ text "Submit" ]


viewEditStuff : Maybe UserInputWithId -> List Category -> List Product -> Html Msg
viewEditStuff maybeStuff cats prods =
    div []
        [ h3 [] [ text "Edit a product" ]
        , pickAProduct prods (UpdatedEdit << pickProductByStringId prods)
        , showEditInput maybeStuff cats
        ]


pickAProduct : List Product -> (String -> msg) -> Html msg
pickAProduct prods msg =
    select [ onInput msg ]
        (option [ value "Nothing", selected True ] [ text "Select..." ] :: Form.Product.prodsToOptions prods)


pickProductByStringId : List Product -> String -> Maybe Product
pickProductByStringId prods stringId =
    List.filter (\p -> String.fromInt p.id == stringId) prods
        |> List.head


showEditInput : Maybe UserInputWithId -> List Category -> Html Msg
showEditInput maybeUserInput cats =
    case maybeUserInput of
        Nothing ->
            h3 [] [ text "Pick something to edit!" ]

        Just ( uInput, id ) ->
            div []
                [ Form.Product.productForm cats uInput UpdateUserInput
                , submitButton uInput (Edit id)
                ]


viewDeleteStuff : Bool -> Maybe Int -> List Product -> Html Msg
viewDeleteStuff isConfirmShowing maybeId prods =
    div []
        [ h3 [] [ text "Delete a product" ]
        , pickAProduct prods (UpdatedDelete << pickProductByStringId prods)
        , showDeleteButton isConfirmShowing maybeId
        ]


showDeleteButton : Bool -> Maybe Int -> Html Msg
showDeleteButton isConfirmShowing maybeId =
    case maybeId of
        Nothing ->
            h3 [] [ text "Pick something to delete!" ]

        Just id ->
            let
                displayButton =
                    if isConfirmShowing then
                        div []
                            [ h3 [] [ text "Are you sure?" ]
                            , button [ onClick <| ToggleConfirm False ] [ text "No!" ]
                            , button [ onClick <| Delete id ] [ text "Yes!" ]
                            ]

                    else
                        div [] []
            in
            div []
                [ button [ onClick <| ToggleConfirm True ] [ text "Delete!" ]
                , displayButton
                ]


type Msg
    = GotCats (WebData (List Category))
    | GotProds (WebData (List Product))
    | NewAction Action
    | UpdateUserInput UserInputProduct
    | UpdatedEdit (Maybe Product)
    | UpdatedDelete (Maybe Product)
    | CreateProduct Product
    | Edit Int Product
    | Delete Int
    | ToggleConfirm Bool
    | Feedback (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCats cats ->
            ( { model | availableCats = cats }, Cmd.none )

        GotProds prods ->
            ( { model | availableProds = prods }, Cmd.none )

        NewAction newAction ->
            ( { model | action = newAction }, Cmd.none )

        UpdateUserInput newInput ->
            case model.action of
                Creating _ ->
                    ( { model | action = Creating newInput }, Cmd.none )

                Editing (Just ( _, oldId )) ->
                    ( { model | action = Editing <| Just ( newInput, oldId ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdatedEdit newProduct ->
            case newProduct of
                Nothing ->
                    ( { model | action = Editing Nothing }, Cmd.none )

                Just prod ->
                    ( { model | action = Editing <| Just ( Products.prodToString prod, prod.id ) }, Cmd.none )

        UpdatedDelete newProduct ->
            case newProduct of
                Nothing ->
                    ( { model | action = Deleting False Nothing }, Cmd.none )

                Just prod ->
                    ( { model | action = Deleting False <| Just prod.id }, Cmd.none )

        CreateProduct prod ->
            ( model, Requests.submitProduct Feedback prod )

        Edit id newContent ->
            ( model, Requests.editProduct Feedback newContent id )

        Delete id ->
            ( model, Requests.removeProduct Feedback id )

        ToggleConfirm state ->
            case model.action of
                Deleting _ id ->
                    ( { model | action = Deleting state id }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Feedback f ->
            case f of
                Err e ->
                    ( { model | status = httpErrorToString e }, Cmd.none )

                Ok m ->
                    refreshModel m


refreshModel : String -> ( Model, Cmd Msg )
refreshModel status =
    let
        ( mdl, cmds ) =
            init
    in
    ( { mdl | status = status }, cmds )
