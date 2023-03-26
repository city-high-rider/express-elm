module Pages.Orders exposing (Model, Msg, init, update, view)

import CheckoutInfo exposing (Info)
import Colorscheme exposing (Colorscheme)
import Element exposing (Element, centerX, column, el, fill, link, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import ErrorViewing exposing (viewHttpErrorStyled)
import Html exposing (Html)
import OrderIds exposing (Order, getOrders)
import Products exposing (Product)
import RemoteData exposing (WebData)
import StyleLabels exposing (buttonLabel, layoutWithHeader, linkLabel)


type alias Model =
    { credentials : Maybe String
    , availableOrders : WebData (List Section)
    }


type alias Section =
    { order : Order
    , showing : Bool
    }


init : Maybe String -> ( Model, Cmd Msg )
init pass =
    ( Model pass RemoteData.Loading
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
                column [ centerX ]
                    [ viewOrders model creds
                    ]


viewOrders : Model -> String -> Element Msg
viewOrders model creds =
    column []
        [ case model.availableOrders of
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
                    (List.map viewOrder orders)
        ]


viewOrder : Section -> Element Msg
viewOrder sec =
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
            column [ Background.color Colorscheme.light.fgDarker ]
                [ viewBundles sec.order.bundles
                ]

          else
            Element.none
        ]


viewBundles : List ( Product, Int ) -> Element msg
viewBundles bundles =
    column []
        (List.map viewBundle bundles)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOrds ords ->
            ( { model | availableOrders = webDataListMap (\o -> Section o False) ords }, Cmd.none )

        ToggleSection sec ->
            let
                fn : Section -> Section
                fn s =
                    if s == sec then
                        { s | showing = not s.showing }

                    else
                        s
            in
            ( { model | availableOrders = webDataListMap fn model.availableOrders }, Cmd.none )


webDataListMap : (a -> b) -> WebData (List a) -> WebData (List b)
webDataListMap fn webdata =
    RemoteData.map (\ws -> List.map fn ws) webdata
