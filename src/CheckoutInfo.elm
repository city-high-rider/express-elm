module CheckoutInfo exposing (Bundle, Info, bundlesEncoder, infoEncoder, verifyInfo)

import Json.Encode as Encode
import Products exposing (Product)


type alias Info =
    { name : String
    , surname : String
    , phone : String
    }


type alias Bundle =
    ( Product, Int )


verifyInfo : Info -> Result String Info
verifyInfo info =
    let
        fname =
            checkNotEmpty info.name "Enter your first name!"

        lname =
            checkNotEmpty info.surname "Enter your last name!"

        phone =
            checkNotEmpty info.phone "Enter your phone so we can contact you about your order!"
    in
    Result.map3 Info fname lname phone


checkNotEmpty : String -> String -> Result String String
checkNotEmpty tgt msg =
    if tgt == "" then
        Err msg

    else
        Ok tgt


infoEncoder : Info -> Encode.Value
infoEncoder info =
    Encode.object
        [ ( "fname", Encode.string info.name )
        , ( "lname", Encode.string info.surname )
        , ( "phone", Encode.string info.phone )
        ]


bundlesEncoder : List Bundle -> Encode.Value
bundlesEncoder bundles =
    Encode.list bundleEncoder bundles


bundleEncoder : Bundle -> Encode.Value
bundleEncoder ( prod, qty ) =
    Encode.object
        [ ( "productId", Products.productIdEncoder prod.id )
        , ( "quantity", Encode.int qty )
        ]
