module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate phrase =
    normalize phrase
        -- Split the string
        |> String.words
        -- Retrieve the first character of each word
        |> List.map (String.left 1)
        -- Join all first characters
        |> String.join ""


{-| Replace '.', '-' and '\_' by spaces and change to upper case.
-}
normalize : String -> String
normalize phrase =
    phrase
        |> String.replace "." " "
        |> String.replace "-" " "
        |> String.replace "_" " "
        |> String.toUpper
        |> String.trim
