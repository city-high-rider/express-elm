module Pages.AdminPageUtils exposing (..)

createSuccessMessage : Result e a -> String -> String
createSuccessMessage result action =
    case result of
        Err _ ->
            "Something went wrong!"

        Ok _ ->
            "successfully " ++ action
