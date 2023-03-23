module Pages.AdminProducts exposing (..)

import Category exposing (Category, getCategories)
import Colorscheme
import Element exposing (Element, centerX, column, el, fill, layout, link, mouseOver, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button, labelAbove)
import ErrorViewing exposing (..)
import Form.Product
import Html exposing (Html)
import Pages.AdminPageUtils exposing (showModelStatusStyle)
import Products exposing (Product, UserInputProduct, getProducts)
import RemoteData exposing (WebData)
import Requests
import ServerResponse exposing (ServerResponse)


type alias Model =
    { action : Action
    , availableCats : WebData (List Category)
    , availableProds : WebData (List Product)
    , status : WebData ServerResponse
    , credentials : Maybe String
    }


type Action
    = NotPicked
    | Creating UserInputProduct
    | Editing (Maybe ( UserInputProduct, Int ))
    | Deleting Bool (Maybe Int)


init : Maybe String -> ( Model, Cmd Msg )
init creds =
    ( { emptyModel | credentials = creds }
    , Cmd.batch [ getCategories GotCats, getProducts GotProds ]
    )


emptyModel : Model
emptyModel =
    Model
        NotPicked
        RemoteData.Loading
        RemoteData.Loading
        RemoteData.NotAsked
        Nothing


view : Model -> Html Msg
view model =
    layout [ Background.color Colorscheme.light.fg, Font.color Colorscheme.light.bg ] <|
        column [ width fill, centerX ]
            [ let
                serverResources =
                    RemoteData.map2 (\c p -> ( c, p )) model.availableCats model.availableProds
              in
              case serverResources of
                RemoteData.NotAsked ->
                    el [] (text "The developer forgot to request resources from server!")

                RemoteData.Loading ->
                    el [] (text "Getting data from the server... please wait!")

                RemoteData.Failure err ->
                    column []
                        [ el [] (text "Unable to get data from the server!")
                        , viewHttpErrorStyled err
                        ]

                RemoteData.Success ( cats, prods ) ->
                    case model.credentials of
                        Nothing ->
                            column [ centerX ]
                                [ el [ centerX, Font.size 30, Font.color Colorscheme.light.primary ] (text "You are not logged in!")
                                , el [] (text "Your requests will not work. Login at")
                                , link
                                    [ Font.color Colorscheme.light.secondary
                                    , mouseOver [ Font.color Colorscheme.light.misc ]
                                    ]
                                    { url = "/login", label = text "Login" }
                                ]

                        Just _ ->
                            column [ spacing 15, centerX ]
                                [ el [ centerX, Font.color Colorscheme.light.primary, Font.size 30 ] (text "What would you like to do?")
                                , showButtons
                                , showRelevantForm model.action cats prods
                                , showModelStatusStyle model.status
                                ]
            ]


showButtons : Element Msg
showButtons =
    row [ spacing 15 ]
        [ button [] { onPress = Just <| NewAction <| Creating Products.empty, label = text "Create new product" }
        , button [] { onPress = Just <| NewAction <| Editing Nothing, label = text "Edit a product" }
        , button [] { onPress = Just <| NewAction <| Deleting False Nothing, label = text "Remove a product" }
        ]


showRelevantForm : Action -> List Category -> List Product -> Element Msg
showRelevantForm action cats prods =
    column [ centerX ]
        [ case action of
            NotPicked ->
                el [ Font.size 25 ] (text "Give me something to do!")

            Creating uInput ->
                creatingForm uInput cats

            Editing maybeStuff ->
                viewEditStuff maybeStuff cats prods

            Deleting isConfirmShowing maybeId ->
                viewDeleteStuff isConfirmShowing maybeId prods
        ]


creatingForm : UserInputProduct -> List Category -> Element Msg
creatingForm uInput cats =
    column [ centerX, spacing 10 ]
        [ el [ Font.size 25 ] (text "Create a product")
        , Form.Product.productForm cats uInput UpdateUserInput
        , submitButton uInput CreateProduct
        ]


submitButton : UserInputProduct -> (Product -> msg) -> Element msg
submitButton userInput msg =
    case Products.userInputToProduct userInput of
        Err e ->
            paragraph []
                [ el [ Font.color Colorscheme.light.primary ] (text "Error: ")
                , el [ Font.color Colorscheme.light.bg ] (text e)
                ]

        Ok p ->
            button [] { onPress = Just <| msg p, label = text "submit" }


viewEditStuff : Maybe ( UserInputProduct, Int ) -> List Category -> List Product -> Element Msg
viewEditStuff maybeStuff cats prods =
    column [ centerX ]
        [ el [ Font.size 25 ] (text "Edit a product")
        , pickAProduct prods (Maybe.map Tuple.second maybeStuff) UpdatedEdit
        , showEditInput maybeStuff cats
        ]


pickAProduct : List Product -> Maybe Int -> (Product -> msg) -> Element msg
pickAProduct prods selected msg =
    Input.radio []
        { onChange = msg
        , options = List.map (\p -> Input.option p <| text p.name) prods
        , selected =
            case selected of
                Nothing ->
                    Nothing

                Just s ->
                    List.head <| List.filter (\p -> p.id == s) prods
        , label = labelAbove [] (text "Product to edit")
        }


showEditInput : Maybe ( UserInputProduct, Int ) -> List Category -> Element Msg
showEditInput maybeProd cats =
    case maybeProd of
        Nothing ->
            el [] (text "Pick something to edit!")

        Just ( uInput, id ) ->
            column []
                [ Form.Product.productForm cats uInput UpdateUserInput
                , submitButton uInput (Edit id)
                ]


viewDeleteStuff : Bool -> Maybe Int -> List Product -> Element Msg
viewDeleteStuff isConfirmShowing maybeId prods =
    column [ centerX ]
        [ el [ Font.size 25 ] (text "Delete a product")
        , pickAProduct prods maybeId UpdatedDelete
        , showDeleteButton isConfirmShowing maybeId
        ]


showDeleteButton : Bool -> Maybe Int -> Element Msg
showDeleteButton isConfirmShowing maybeId =
    case maybeId of
        Nothing ->
            el [] (text "Pick something to delete!")

        Just id ->
            let
                displayButton =
                    if isConfirmShowing then
                        column []
                            [ el [] (text "Are you sure?")
                            , button [] { onPress = Just <| ToggleConfirm False, label = text "No!" }
                            , button [] { onPress = Just <| Delete id, label = text "Yes!" }
                            ]

                    else
                        Element.none
            in
            column []
                [ button [] { onPress = Just <| ToggleConfirm True, label = text "Delete!" }
                , displayButton
                ]


type Msg
    = GotCats (WebData (List Category))
    | GotProds (WebData (List Product))
    | NewAction Action
    | UpdateUserInput UserInputProduct
    | UpdatedEdit Product
    | UpdatedDelete Product
    | CreateProduct Product
    | Edit Int Product
    | Delete Int
    | ToggleConfirm Bool
    | Feedback (WebData ServerResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pass =
            Maybe.withDefault "" model.credentials
    in
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
            ( { model | action = Editing <| Just ( Products.prodToString newProduct, newProduct.id ) }, Cmd.none )

        UpdatedDelete newProduct ->
            ( { model | action = Deleting False <| Just newProduct.id }, Cmd.none )

        CreateProduct prod ->
            ( model, Requests.submitProduct pass (RemoteData.fromResult >> Feedback) prod )

        Edit id newContent ->
            ( model, Requests.editProduct pass (RemoteData.fromResult >> Feedback) newContent id )

        Delete id ->
            ( model, Requests.removeProduct pass (RemoteData.fromResult >> Feedback) id )

        ToggleConfirm state ->
            case model.action of
                Deleting _ id ->
                    ( { model | action = Deleting state id }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Feedback f ->
            let
                ( empty, cmds ) =
                    init model.credentials
            in
            ( { empty | status = f }, cmds )
