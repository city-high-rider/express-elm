module OrderIds exposing (OrderIds, getOrders)

import CheckoutInfo exposing (Info, bundlesDecoder, infoDecoder)
import Http
import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)
import Products exposing (ProductId)
import RemoteData exposing (WebData)


type alias OrderIds =
    { info : Info
    , bundles : List ( ProductId, Int )
    , id : Int
    }


getOrders : (WebData (List OrderIds) -> msg) -> Cmd msg
getOrders msg =
    Http.get
        { url = "http://localhost:3000/getOrders"
        , expect = Http.expectJson (RemoteData.fromResult >> msg) orderIdsDecoder
        }


orderIdsDecoder : Decoder (List OrderIds)
orderIdsDecoder =
    Decode.list orderIdDecoder


orderIdDecoder : Decoder OrderIds
orderIdDecoder =
    Decode.succeed OrderIds
        |> required "info" infoDecoder
        |> required "bundles" bundlesDecoder
        |> required "id" int
