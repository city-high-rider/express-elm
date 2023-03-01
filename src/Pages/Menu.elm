module Pages.Menu exposing (..)

import Category exposing (Category, CategoryId, getCategories)
import ErrorViewing exposing (viewHttpError)
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onCheck)
import Http
import Products exposing (Product, productsDecoder)
import RemoteData exposing (WebData)



-- init and model


type alias Model =
    { categories : WebData (List Category) -- these are the available categories to pick food from
    , sections : List Section -- list of sections that are currently displayed
    }



-- you will see categories referred to as "cats" extensively
{- A section consists of a category and its corresponding food items -}


type alias Section =
    { category : Category
    , products : WebData (List Product)
    }


init : ( Model, Cmd Msg )
init =
    ( Model RemoteData.Loading []
    , getCategories GotCats
    )



-- view function


view : Model -> Html Msg
view model =
    div []
        [ viewChecks model
        , viewSections model.sections
        ]


viewChecks : Model -> Html Msg
viewChecks model =
    case model.categories of
        RemoteData.NotAsked ->
            div [] [ h2 [] [ text "You haven't asked for the data!" ] ]

        RemoteData.Loading ->
            h2 [] [ text "Loading.. please wait" ]

        RemoteData.Failure err ->
            div []
                [ h2 [] [ text "someting went wrong" ]
                , viewHttpError err
                ]

        RemoteData.Success cats ->
            div []
                [ makeCheckmarks cats
                ]


viewSections : List Section -> Html Msg
viewSections sections =
    div [] (List.map viewSection sections)


viewSection : Section -> Html Msg
viewSection section =
    div []
        [ h2 [] [ text section.category.name ]
        , viewProds section.products section.category.units
        ]


makeCheckmarks : List Category -> Html Msg
makeCheckmarks cats =
    div []
        (List.map makeCheckmark cats)


makeCheckmark : Category -> Html Msg
makeCheckmark cat =
    div []
        [ span [] [ text cat.name ]
        , input [ type_ "checkbox", onCheck (CheckedCat cat) ] []
        ]


viewProds : WebData (List Product) -> String -> Html Msg
viewProds prods units =
    case prods of
        RemoteData.NotAsked ->
            p [] [ text "haven't asked" ]

        RemoteData.Loading ->
            p [] [ text "Getting your items... please wait" ]

        RemoteData.Failure err ->
            div []
                [ h2 [] [ text "unable to get your item!" ]
                , viewHttpError err
                ]

        RemoteData.Success goodProds ->
            div [] (List.map (viewProd units) goodProds)


viewProd : String -> Product -> Html Msg
viewProd units product =
    let
        productCost =
            String.fromFloat <| (toFloat product.price / 100)
    in
    div []
        [ h3 [] [ text product.name ]
        , p [] [ text ("Description: " ++ product.description) ]
        , p [] [ text ("Cost : " ++ productCost) ]
        , p [] [ text (String.fromInt product.size ++ units) ]
        ]



-- update function


getProducts : CategoryId -> Cmd Msg
getProducts catId =
    Http.get
        { url = "http://localhost:3000/menu/" ++ (String.fromInt <| Category.catIdToInt catId)
        , expect = Http.expectJson (RemoteData.fromResult >> GotProducts catId) productsDecoder
        }


type Msg
    = GotProducts CategoryId (WebData (List Product))
    | GotCats (WebData (List Category))
    | CheckedCat Category Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts forCat prods ->
            ( { model | sections = insertProducts prods forCat model.sections }, Cmd.none )

        GotCats cats ->
            ( { model | categories = cats }, Cmd.none )

        CheckedCat cat on ->
            let
                newSections =
                    if on then
                        Section cat RemoteData.Loading :: model.sections

                    else
                        List.filter (\s -> s.category /= cat) model.sections
            in
            ( { model
                | sections = newSections
              }
            , updateSections newSections
            )


insertProducts : WebData (List Product) -> CategoryId -> List Section -> List Section
insertProducts newProducts id oldSections =
    List.map
        (\s ->
            if s.category.id == id then
                { s | products = newProducts }

            else
                s
        )
        oldSections


updateSections : List Section -> Cmd Msg
updateSections sections =
    List.map updateSection sections
        |> Cmd.batch


updateSection : Section -> Cmd Msg
updateSection section =
    getProducts section.category.id


