module Form.Checkout exposing (CheckoutInfo(..), CheckoutInput, ContactMethod(..), checkoutForm)

import Element exposing (Element, column, el, text)
import Element.Input as Input exposing (labelLeft, option)


type CheckoutInfo
    = NotAsked
    | Unverified CheckoutInput


type ContactMethod
    = NotSelected
    | Phone
    | Email
    | Other


type alias CheckoutInput =
    { name : String
    , surname : String
    , contact : ContactMethod
    , contactInfo : String
    }


checkoutForm : (CheckoutInput -> msg) -> CheckoutInput -> Element msg
checkoutForm msg input =
    column []
        [ Input.text []
            { onChange = msg << updateName input
            , text = input.name
            , placeholder = Nothing
            , label = labelLeft [] (text "Your name")
            }
        , Input.text []
            { onChange = msg << updateSurname input
            , text = input.surname
            , placeholder = Nothing
            , label = labelLeft [] (text "Your surname")
            }
        , Input.radio []
            { onChange = msg << updateContact input
            , options = [ option Phone (text "Phone"), option Email (text "Email"), option Other (text "Other (Specify details in textbox)") ]
            , selected =
                case input.contact of
                    NotSelected ->
                        Nothing

                    _ ->
                        Just input.contact
            , label = labelLeft [] (text "Contact method")
            }
        , case input.contact of
            NotSelected ->
                el [] (text "Please select a contact method")

            _ ->
                Input.text []
                    { onChange = msg << updateContactInfo input
                    , text = input.contactInfo
                    , placeholder = Nothing
                    , label = labelLeft [] (text "How will we contact you?")
                    }
        ]


updateContactInfo : CheckoutInput -> String -> CheckoutInput
updateContactInfo old info =
    { old | contactInfo = info }


updateName : CheckoutInput -> String -> CheckoutInput
updateName old newName =
    { old | name = newName }


updateSurname : CheckoutInput -> String -> CheckoutInput
updateSurname old newName =
    { old | surname = newName }


updateContact : CheckoutInput -> ContactMethod -> CheckoutInput
updateContact old newCon =
    { old | contact = newCon }
