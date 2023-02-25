module CategoryList exposing (..)
import RemoteData exposing (WebData)
import Pages.Menu exposing (CategoryId)

type alias Model =
    { categories : WebData (List CategoryId)
    }


init : (Model, Cmd Msg)
init =
    (Model RemoteData.Loading, getCategories)


-- view

view : Model -> Html Msg
view model =
    case model.categories of 
        NotAsked ->
            p [] [text "You haven't asked for anything"]

        Loading -> 
            h3 [] [text "loading.. please wait"]
