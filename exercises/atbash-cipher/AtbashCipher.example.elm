module AtbashCipher exposing (decode, encode)

import Char
import Dict
import String
import Tuple


encode : String -> String
encode plain =
    let
        translate =
            toTranslator alphabet reversedAlphabet
    in
    plain
        |> String.toLower
        |> String.filter (\c -> Char.isLower c || Char.isDigit c)
        |> String.map translate
        |> groupBy 5
        |> String.join " "


decode : String -> String
decode cipher =
    let
        translate =
            toTranslator reversedAlphabet alphabet
    in
    cipher
        |> String.filter ((/=) ' ')
        |> String.map translate


alphabet : String
alphabet =
    "abcdefghijklmnopqrstuvwxyz"


reversedAlphabet : String
reversedAlphabet =
    String.reverse alphabet


toTranslator : String -> String -> Char -> Char
toTranslator from to =
    let
        table =
            List.map2 Tuple.pair (String.toList from) (String.toList to)
                |> Dict.fromList

        translate key =
            Dict.get key table
                |> Maybe.withDefault key
    in
    translate


groupBy : Int -> String -> List String
groupBy n string =
    case splitAt n string of
        ( start, "" ) ->
            [ start ]

        ( start, end ) ->
            start :: groupBy n end


splitAt : Int -> String -> ( String, String )
splitAt n string =
    ( String.left n string, String.dropLeft n string )
