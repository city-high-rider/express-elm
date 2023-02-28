module Pages.Create exposing (..)

import Category exposing (Category)
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import Http


type alias Model =
    { catToSubmit : Category
    }


init : ( Model, Cmd Msg )
init =
    ( Model Category.empty, Cmd.none )


view : Model -> Html Msg
view _ =
    Html.form []
        [ div []
            [ text "Category name"
            , br [] []
            , input [ type_ "text", onInput StoreName ] []
            ]
        , br [] []
        , div []
            [ text "Units of size measurement"
            , br [] []
            , input [ type_ "text", onInput StoreUnits ] []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick Submit ]
                [ text "Submit" ]
            ]
        ]


type Msg
    = StoreName String
    | StoreUnits String
    | Submit
    | CatCreated (Result Http.Error Category)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreName val ->
            let
                oldCat =
                    model.catToSubmit

                newCat =
                    { oldCat | name = val }
            in
            ( { model | catToSubmit = newCat }, Cmd.none )

        StoreUnits val ->
            let
                oldCat =
                    model.catToSubmit

                newCat =
                    { oldCat | units = val }
            in
            ( { model | catToSubmit = newCat }, Cmd.none )

        Submit ->
            ( model, submitResult model.catToSubmit )

        CatCreated _ ->
            ( model, Cmd.none )


submitResult : Category -> Cmd Msg
submitResult cat =
    Http.post
        { url = "http://localhost:3000/newCat"
        , body = Http.jsonBody (Category.newCatEncoder cat)
        , expect = Http.expectJson CatCreated Category.catDecoder
        }
