module Pages.Menu exposing (..)

import Category exposing (Category, CategoryId, getCategories)
import Colorscheme exposing (Colorscheme)
import Element exposing (Element, centerX, column, el, fill, height, layout, mouseOver, paragraph, row, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, checkbox)
import ErrorViewing exposing (viewHttpErrorStyled)
import Html exposing (Html)
import Products exposing (Product, getProducts)
import RemoteData exposing (WebData)
import StyleLabels exposing (layoutWithHeader)



-- init and model


type alias Order =
    ( Product, Int )


type alias Model =
    { products : WebData (List Product) -- list of sections that are currently displayed
    , sections : WebData (List Section)
    , cart : List Order
    }



-- you will see categories referred to as "cats" extensively
{- A section is simply a category of products that is either showing or not showing. -}


type alias Section =
    { category : Category
    , showing : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model RemoteData.Loading RemoteData.Loading []
    , Cmd.batch [ getProducts GotProds, getCategories GotCats ]
    )



-- view function


view : Model -> Html Msg
view model =
    layoutWithHeader [ Background.color Colorscheme.light.fg ] <|
        let
            sectionsAndProducts =
                RemoteData.map2 (\s p -> ( s, p )) model.sections model.products
        in
        case sectionsAndProducts of
            RemoteData.NotAsked ->
                el [] (text "Developer forgot to send an Http request lol")

            RemoteData.Loading ->
                el [] (text "Loading data... please wait!")

            RemoteData.Failure err ->
                column []
                    [ el [] (text "Unable to get data from the server!")
                    , viewHttpErrorStyled err
                    ]

            RemoteData.Success ( sections, products ) ->
                column [ spaceEvenly, spacing 20, centerX, width fill ]
                    [ viewSections sections products
                    , viewCart model.cart
                    ]


viewSections : List Section -> List Product -> Element Msg
viewSections sections products =
    column
        [ spacing 30
        , width <| Element.maximum 1200 fill
        , centerX
        , Element.padding 20
        ]
        (List.map (viewSection products) sections)


viewSection : List Product -> Section -> Element Msg
viewSection prods section =
    column
        [ width fill, Background.color Colorscheme.light.fgDarker ]
        [ checkbox
            [ centerX
            , Background.color Colorscheme.light.bg
            , Border.rounded 8
            , Border.solid
            ]
            { onChange = ToggleSection section
            , icon = Element.Input.defaultCheckbox
            , checked = section.showing
            , label =
                Element.Input.labelRight
                    [ Font.color Colorscheme.light.primary
                    , Font.size 35
                    , centerX
                    ]
                    (text section.category.name)
            }
        , if section.showing then
            viewProds (List.filter (\p -> p.category == section.category.id) prods) section.category.units

          else
            Element.none
        ]


viewCart : List Order -> Element Msg
viewCart orders =
    case orders of
        [] ->
            el [ Font.color Colorscheme.light.bg ] (text "You haven't ordered anything yet!")

        ords ->
            column [ Font.color Colorscheme.light.bg ]
                [ el [] (text "You have ordered:")
                , viewOrders ords
                , paragraph []
                    [ el [] (text "Your total comes out to: ")
                    , el [ Font.color Colorscheme.light.primary ] <|
                        text <|
                            String.fromFloat <|
                                (\x -> x / 100) <|
                                    toFloat <|
                                        List.sum <|
                                            List.map (\( p, q ) -> p.price * q) ords
                    ]
                ]


viewOrders : List Order -> Element Msg
viewOrders orders =
    column [] (List.map viewOrder orders)


viewOrder : Order -> Element Msg
viewOrder ( prod, qty ) =
    row []
        [ el [] (text (String.fromInt qty ++ "x " ++ prod.name))
        , button []
            { onPress = Just <| AddOrder ( prod, -1 )
            , label =
                el
                    [ Element.padding 6
                    , Font.color Colorscheme.light.secondary
                    , Background.color Colorscheme.light.fgDarker
                    ]
                    (text "Remove")
            }
        ]


viewProds : List Product -> String -> Element Msg
viewProds prods units =
    wrappedRow
        [ Background.color Colorscheme.light.fgDarker
        , width fill
        , spacing 15
        , spaceEvenly
        ]
        (List.map (viewProd units) prods)


viewProd : String -> Product -> Element Msg
viewProd units product =
    let
        productCost =
            String.fromFloat <| (toFloat product.price / 100)
    in
    column
        [ Font.color Colorscheme.light.bg
        , width fill
        , Border.color Colorscheme.light.misc
        , Border.dashed
        , Border.width 2
        , Background.color Colorscheme.light.fg
        ]
        [ el [ Font.size 30, centerX ] (text product.name)
        , el [] (text ("Description: " ++ product.description))
        , el [] (text ("Cost : " ++ productCost))
        , el [] (text (String.fromInt product.size ++ units))
        , button []
            { onPress = Just <| AddOrder ( product, 1 )
            , label =
                el
                    [ Font.color Colorscheme.light.primary
                    , Element.padding 8
                    , Background.color Colorscheme.light.bg
                    ]
                    (text "Add to cart")
            }
        ]



-- update function


type Msg
    = GotProds (WebData (List Product))
    | GotCats (WebData (List Category))
    | ToggleSection Section Bool
    | AddOrder Order


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProds prods ->
            ( { model | products = prods }, Cmd.none )

        -- our model does not work with categories, just sections. So we
        -- must convert.
        GotCats cats ->
            ( { model | sections = webDataListMap (\c -> Section c False) cats }, Cmd.none )

        ToggleSection section state ->
            ( { model
                | sections =
                    webDataListMap
                        (\s ->
                            if s == section then
                                { s | showing = state }

                            else
                                s
                        )
                        model.sections
              }
            , Cmd.none
            )

        AddOrder order ->
            ( { model | cart = mergeOrder model.cart order }, Cmd.none )



-- there's gotta be a more elegant way to do this... function composition or something.
-- I feel like I'm missing something obvious here.


webDataListMap : (a -> b) -> WebData (List a) -> WebData (List b)
webDataListMap fn webdata =
    RemoteData.map (\ws -> List.map fn ws) webdata


mergeOrder : List Order -> Order -> List Order
mergeOrder orders ( prod, qty ) =
    case orders of
        [] ->
            [ ( prod, qty ) ]

        ( cp, cq ) :: xs ->
            if cp == prod && cq + qty > 0 then
                ( cp, cq + qty ) :: xs

            else if cq + qty <= 0 then
                xs

            else
                ( cp, cq ) :: mergeOrder xs ( prod, qty )
