module Form.Checkout exposing (infoForm)

import CheckoutInfo exposing (Info, verifyInfo)
import Element exposing (Element, column, text)
import Element.Input as Input exposing (labelLeft)


infoForm : (Info -> msg) -> Info -> Element msg
infoForm msg oldInfo =
    column []
        [ Input.text []
            { onChange = msg << updateName oldInfo
            , placeholder = Nothing
            , text = oldInfo.name
            , label = labelLeft [] (text "Your first name")
            }
        , Input.text []
            { onChange = msg << updateLastname oldInfo
            , placeholder = Nothing
            , text = oldInfo.surname
            , label = labelLeft [] (text "Your last name")
            }
        , Input.text []
            { onChange = msg << updatePhone oldInfo
            , placeholder = Nothing
            , text = oldInfo.phone
            , label = labelLeft [] (text "Your phone number")
            }
        ]


updateName : Info -> String -> Info
updateName old newName =
    { old | name = newName }


updateLastname : Info -> String -> Info
updateLastname old newName =
    { old | surname = newName }


updatePhone : Info -> String -> Info
updatePhone old newPhone =
    { old | phone = newPhone }
