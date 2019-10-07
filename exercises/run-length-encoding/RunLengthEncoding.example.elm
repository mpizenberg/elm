module RunLengthEncoding exposing (decode, encode)

-- Encode


encode : String -> String
encode input =
    String.foldr encodeAccum ( 0, ' ', [] ) input
        |> (\( count, lastChar, groups ) -> String.concat (encodeGroup count lastChar :: groups))


encodeAccum : Char -> ( Int, Char, List String ) -> ( Int, Char, List String )
encodeAccum c ( count, lastChar, encodedSoFar ) =
    if c == lastChar then
        ( count + 1, lastChar, encodedSoFar )

    else
        ( 1, c, encodeGroup count lastChar :: encodedSoFar )


encodeGroup : Int -> Char -> String
encodeGroup count c =
    if count == 0 then
        ""

    else if count == 1 then
        String.fromChar c

    else
        String.fromInt count ++ String.fromChar c



-- Decode


decode : String -> String
decode input =
    String.foldl decodeAccum ( 0, [] ) input
        |> (\( _, decodedSoFar ) -> String.concat decodedSoFar)
        |> String.reverse


decodeAccum : Char -> ( Int, List String ) -> ( Int, List String )
decodeAccum c ( count, decodedSoFar ) =
    if Char.isDigit c then
        ( incrementCount c count, decodedSoFar )

    else
        ( 0, decodeGroup count c :: decodedSoFar )


incrementCount : Char -> Int -> Int
incrementCount c count =
    String.fromChar c
        |> String.toInt
        |> Maybe.map (\n -> 10 * count + n)
        |> Maybe.withDefault count


decodeGroup : Int -> Char -> String
decodeGroup count c =
    if count == 0 then
        String.fromChar c

    else
        String.repeat count (String.fromChar c)
