module OrderIds exposing (Order, getOrders)

import CheckoutInfo exposing (Info, infoDecoder)
import Http
import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)
import Products exposing (Product, productDecoder)
import RemoteData exposing (WebData)


type alias Order =
    { id : Int
    , info : Info
    , bundles : List ( Product, Int )
    }


getOrders : (WebData (List Order) -> msg) -> Cmd msg
getOrders msg =
    Http.get
        { url = "http://localhost:3000/getOrders"
        , expect = Http.expectJson (RemoteData.fromResult >> msg) ordersDecoder
        }


ordersDecoder : Decoder (List Order)
ordersDecoder =
    Decode.list orderDecoder


orderDecoder : Decoder Order
orderDecoder =
    Decode.succeed Order
        |> required "id" int
        |> required "info" infoDecoder
        |> required "bundles" bundlesDecoder


bundlesDecoder : Decoder (List ( Product, Int ))
bundlesDecoder =
    Decode.list bundleDecoder


bundleDecoder : Decoder ( Product, Int )
bundleDecoder =
    Decode.succeed (\p q -> ( p, q ))
        |> required "product" productDecoder
        |> required "quantity" int
