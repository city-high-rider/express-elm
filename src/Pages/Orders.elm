module Pages.Orders exposing (Model, Msg, init, update, view)

import CheckoutInfo exposing (Info)
import Colorscheme exposing (Colorscheme)
import Element exposing (Element, centerX, column, el, fill, link, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button, labelAbove)
import ErrorViewing exposing (viewHttpErrorStyled)
import Html exposing (Html)
import OrderIds exposing (Order, getOrders)
import Pages.AdminPageUtils exposing (showModelStatusStyle)
import Products exposing (Product)
import RemoteData exposing (WebData)
import Requests exposing (rejectOrder)
import ServerResponse exposing (ServerResponse)
import StyleLabels exposing (buttonLabel, layoutWithHeader, linkLabel)


type alias Model =
    { credentials : Maybe String
    , availableSections : WebData (List Section)
    , status : WebData ServerResponse
    }


type alias Section =
    { order : Order
    , showing : Bool
    , rejectReason : Maybe String
    }


init : Maybe String -> ( Model, Cmd Msg )
init pass =
    ( Model pass RemoteData.Loading RemoteData.NotAsked
    , getOrders GotOrds
    )


view : Model -> Html Msg
view model =
    layoutWithHeader [ Font.color Colorscheme.light.bg, Background.color Colorscheme.light.fg ] <|
        case model.credentials of
            Nothing ->
                column [ centerX ]
                    [ el [ Font.size 30, Font.color Colorscheme.light.primary ] (text "You are not logged in!")
                    , paragraph [ centerX ]
                        [ link [] { url = "/login", label = linkLabel "Log in " [] }
                        , el [] (text "to manage orders")
                        ]
                    ]

            Just creds ->
                column [ centerX, spacing 20 ]
                    [ el [ Font.size 25, Font.color Colorscheme.light.primary ] (showModelStatusStyle model.status)
                    , viewOrders model creds
                    ]


viewOrders : Model -> String -> Element Msg
viewOrders model creds =
    column []
        [ case model.availableSections of
            RemoteData.NotAsked ->
                el [] (text "developer forgot to send http request")

            RemoteData.Loading ->
                el [] (text "Getting data from the server... please wait!")

            RemoteData.Failure e ->
                column []
                    [ el [] (text "Something went wrong!")
                    , viewHttpErrorStyled e
                    ]

            RemoteData.Success orders ->
                column [ spacing 10 ]
                    (List.map (viewOrder creds) orders)
        ]


viewOrder : String -> Section -> Element Msg
viewOrder creds sec =
    column []
        [ row [ width fill ]
            [ column [ width fill ]
                [ paragraph []
                    [ el [ Font.color Colorscheme.light.primary ] (text "Name: ")
                    , el [] (text sec.order.info.name)
                    ]
                , paragraph []
                    [ el [ Font.color Colorscheme.light.secondary ] (text "Surname: ")
                    , el [] (text sec.order.info.surname)
                    ]
                , paragraph []
                    [ el [ Font.color Colorscheme.light.misc ] (text "Phone: ")
                    , el [] (text sec.order.info.phone)
                    ]
                ]
            , button [] { onPress = Just <| ToggleSection sec, label = buttonLabel "expand" [] }
            ]
        , if sec.showing then
            column [ Background.color Colorscheme.light.fgDarker, width fill ]
                [ viewBundles creds sec sec.order.bundles
                ]

          else
            Element.none
        ]


showRejectForm : String -> Section -> Element Msg
showRejectForm pass sec =
    case sec.rejectReason of
        Nothing ->
            Element.none

        Just s ->
            column []
                [ Input.text []
                    { onChange = UpdateReject sec
                    , text = s
                    , placeholder = Nothing
                    , label = labelAbove [] (text "Why?")
                    }
                , showRejectOrError pass sec s
                ]


showRejectOrError : String -> Section -> String -> Element Msg
showRejectOrError pass section reason =
    if reason == "" then
        el [] (text "Please specify a reason!")

    else
        button []
            { onPress = Just <| RejectOrder pass section.order.id reason
            , label = buttonLabel "Go!" []
            }


viewBundles : String -> Section -> List ( Product, Int ) -> Element Msg
viewBundles pass parent bundles =
    column []
        (List.map viewBundle bundles
            ++ [ button []
                    { onPress = Just <| StartRejecting parent
                    , label = buttonLabel "Reject" []
                    }
               , showRejectForm pass parent
               , button []
                    { onPress = Nothing
                    , label = buttonLabel "Mark complete" []
                    }
               ]
        )


viewBundle : ( Product, Int ) -> Element msg
viewBundle ( prod, qty ) =
    row []
        [ paragraph []
            [ el [ Font.color Colorscheme.light.primary ] (text <| String.fromInt qty ++ " of ")
            , el [] (text prod.name)
            ]
        ]


type Msg
    = GotOrds (WebData (List Order))
    | ToggleSection Section
    | StartRejecting Section
    | UpdateReject Section String
    | RejectOrder String Int String
    | ServerResponse (WebData ServerResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOrds ords ->
            ( { model | availableSections = webDataListMap (\o -> Section o False Nothing) ords }, Cmd.none )

        ToggleSection sec ->
            let
                fn : Section -> Section
                fn s =
                    if s == sec then
                        { s | showing = not s.showing }

                    else
                        s
            in
            ( { model | availableSections = webDataListMap fn model.availableSections }, Cmd.none )

        StartRejecting sec ->
            let
                fn : Section -> Section
                fn s =
                    if s == sec then
                        case s.rejectReason of
                            Nothing ->
                                { s | rejectReason = Just "" }

                            Just _ ->
                                { s | rejectReason = Nothing }

                    else
                        s
            in
            ( { model | availableSections = webDataListMap fn model.availableSections }, Cmd.none )

        UpdateReject sec reason ->
            let
                fn : Section -> Section
                fn s =
                    if s == sec then
                        { s | rejectReason = Just reason }

                    else
                        s
            in
            ( { model | availableSections = webDataListMap fn model.availableSections }, Cmd.none )

        RejectOrder pass id reason ->
            ( model, rejectOrder ServerResponse pass id reason )

        ServerResponse res ->
            ( { model | status = res }, getOrders GotOrds )


webDataListMap : (a -> b) -> WebData (List a) -> WebData (List b)
webDataListMap fn webdata =
    RemoteData.map (\ws -> List.map fn ws) webdata
