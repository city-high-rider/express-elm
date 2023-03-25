module Pages.Orders exposing (Model, Msg, init, update, view)

import CheckoutInfo exposing (Info)
import Element exposing (Element, column, el, layout, link, paragraph, text)
import ErrorViewing exposing (viewHttpErrorStyled)
import Html exposing (Html)
import OrderIds exposing (OrderIds, getOrders)
import Products exposing (Product, ProductId, getProducts)
import RemoteData exposing (WebData)
import StyleLabels exposing (linkLabel)


type alias Model =
    { credentials : Maybe String
    , availableProducts : WebData (List Product)
    , availableOrders : WebData (List OrderIds)
    }


type OrderFilled
    = Unresolved


init : Maybe String -> ( Model, Cmd Msg )
init pass =
    ( Model pass RemoteData.Loading RemoteData.Loading
    , Cmd.batch [ getProducts GotProds, getOrders GotOrds ]
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
        [ case RemoteData.map2 (\o p -> ( o, p )) model.availableOrders model.availableProducts of
            RemoteData.NotAsked ->
                el [] (text "developer forgot to send http request")

            RemoteData.Loading ->
                el [] (text "Getting data from the server... please wait!")

            RemoteData.Failure e ->
                column []
                    [ el [] (text "Something went wrong!")
                    , viewHttpErrorStyled e
                    ]

            RemoteData.Success ( o, p ) ->
                column []
                    [ List.map viewPopulatedOrds (populateOrds o p)
                    ]
        ]


populateOrds : List OrderIds -> List Product -> List OrderFilled
populateOrds orderIds products =
    List.map (populateOrd products) orderIds


type Msg
    = GotProds (WebData (List Product))
    | GotOrds (WebData (List OrderIds))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProds prods ->
            ( { model | availableProducts = prods }, Cmd.none )

        GotOrds ords ->
            ( { model | availableOrders = ords }, Cmd.none )
