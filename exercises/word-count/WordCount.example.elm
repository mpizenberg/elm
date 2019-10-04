module WordCount exposing (wordCount)

import Dict exposing (Dict)


wordCount : String -> Dict String Int
wordCount sentence =
    depunctuate sentence
        |> String.toLower
        |> String.words
        |> List.foldl (\w d -> Dict.update w incrMaybe d) Dict.empty


depunctuate : String -> String
depunctuate =
    String.map
        (\char ->
            if Char.isAlphaNum char then
                char

            else
                ' '
        )


incrMaybe : Maybe Int -> Maybe Int
incrMaybe maybe =
    Maybe.withDefault 0 maybe + 1 |> Just
