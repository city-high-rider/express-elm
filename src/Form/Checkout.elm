module Form.Checkout exposing (Info, infoForm, verifyInfo)

import Element exposing (Element, column, text)
import Element.Input as Input exposing (labelLeft)


type alias Info =
    { name : String
    , surname : String
    , phone : String
    }


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
