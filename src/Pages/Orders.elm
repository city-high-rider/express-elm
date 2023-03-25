module Pages.Orders exposing (Model, Msg, init, update, view)

import CheckoutInfo exposing (Info)
import Element exposing (Element, column, el, layout, link, paragraph, text)
import ErrorViewing exposing (viewHttpErrorStyled)
import Html exposing (Html)
import OrderIds exposing (Order, getOrders)
import Products exposing (Product, ProductId, getProducts)
import RemoteData exposing (WebData)
import StyleLabels exposing (linkLabel)


type alias Model =
    { credentials : Maybe String
    , availableOrders : WebData (List Order)
    }


init : Maybe String -> ( Model, Cmd Msg )
init pass =
    ( Model pass RemoteData.Loading
    , getOrders GotOrds
    )


view : Model -> Html Msg
view model =
    layout [] <|
        case model.credentials of
            Nothing ->
                column []
                    [ el [] (text "You are not logged in!")
                    , paragraph []
                        [ link [] { url = "/login", label = linkLabel "Log in" [] }
                        , el [] (text "to manage orders")
                        ]
                    ]

            Just creds ->
                column []
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
                column []
                    (List.map viewOrder orders)
        ]


viewOrder : Order -> Element Msg
viewOrder order =
    column []
        [ paragraph []
            [ el [] (text "Name: ")
            , el [] (text order.info.name)
            ]
        , paragraph []
            [ el [] (text "Surname: ")
            , el [] (text order.info.surname)
            ]
        , paragraph []
            [ el [] (text "Phone: ")
            , el [] (text order.info.phone)
            ]
        ]


type Msg
    = GotOrds (WebData (List Order))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotOrds ords ->
            ( { model | availableOrders = ords }, Cmd.none )
