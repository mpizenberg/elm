module PhoneNumber exposing (getNumber)


getNumber : String -> Maybe String
getNumber input =
    String.filter (\c -> c /= ' ') input
        |> parse phoneNumber
        |> Result.toMaybe
        |> Maybe.map phoneString


type alias Phone =
    { countryCode : Maybe Int
    , areaCode : List Int
    , exchangeCode : List Int
    , subscriberNumber : List Int
    }


phoneString : Phone -> String
phoneString p =
    [ p.areaCode, p.exchangeCode, p.subscriberNumber ]
        |> List.concat
        |> List.map String.fromInt
        |> String.concat


phoneNumber : Parser Phone
phoneNumber =
    succeed Phone
        |> keep (optional countryCode)
        |> ignore (optional separator)
        |> keep areaCode
        |> ignore (optional separator)
        |> keep exchangeCode
        |> ignore (optional separator)
        |> keep subscriberNumber
        |> ignore end


countryCode : Parser Int
countryCode =
    oneOf
        [ succeed identity
            |> ignore (char '+')
            |> keep digitOne
        , digitOne
        ]


areaCode : Parser (List Int)
areaCode =
    oneOf
        [ succeed identity
            |> ignore (char '(')
            |> keep exchangeCode
            |> ignore (char ')')
        , exchangeCode
        ]


exchangeCode : Parser (List Int)
exchangeCode =
    succeed (\x y z -> [ x, y, z ])
        |> keep (check (\x -> x > 1) digit)
        |> keep digit
        |> keep digit


subscriberNumber : Parser (List Int)
subscriberNumber =
    succeed (\x y z w -> [ x, y, z, w ])
        |> keep digit
        |> keep digit
        |> keep digit
        |> keep digit


separator : Parser Char
separator =
    oneOf [ char '-', char '.' ]


end : Parser ()
end input =
    case input of
        "" ->
            ( "", Ok () )

        _ ->
            ( input, Err () )



-- Parser helper #####################################################


type alias Parser a =
    StateM String (Result () a)


parse : Parser a -> String -> Result () a
parse parser input =
    Tuple.second (parser input)



-- Basic parsers


succeed : a -> Parser a
succeed a =
    putValue (Ok a)


fail : Parser a
fail =
    putValue (Err ())


char : Char -> Parser Char
char c =
    check ((==) c) anyChar


digit : Parser Int
digit =
    transform charToDigit anyChar


digitOne : Parser Int
digitOne =
    map (always 1) (char '1')


charToDigit : Char -> Result () Int
charToDigit =
    String.fromChar >> String.toInt >> Result.fromMaybe ()


anyChar : Parser Char
anyChar input =
    case String.uncons input of
        Just ( c, rest ) ->
            ( rest, Ok c )

        Nothing ->
            ( input, Err () )



-- Transform one parser


check : (a -> Bool) -> Parser a -> Parser a
check predicate =
    mapValue (verifyResult predicate)


transform : (a -> Result () b) -> Parser a -> Parser b
transform f pA s =
    let
        ( sA, a ) =
            pA s
    in
    ( sA, Result.andThen f a )


optional : Parser a -> Parser (Maybe a)
optional p input =
    case p input of
        ( rest, Ok a ) ->
            ( rest, Ok (Just a) )

        ( _, Err _ ) ->
            ( input, Ok Nothing )



-- Combine multiple parsers


keep : Parser a -> Parser (a -> b) -> Parser b
keep pA pF =
    map2 (<|) pF pA


ignore : Parser ignore -> Parser keep -> Parser keep
ignore pI pK =
    map2 always pK pI


map : (a -> b) -> Parser a -> Parser b
map f pA =
    mapValue (Result.map f) pA


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f pA pB =
    mapValue2 (Result.map2 f) pA pB


oneOf : List (Parser a) -> Parser a
oneOf parsers =
    case parsers of
        [] ->
            fail

        p :: ps ->
            orElse (oneOf ps) p


orElse : Parser a -> Parser a -> Parser a
orElse second first input =
    case first input of
        ( _, Ok _ ) as okFirst ->
            okFirst

        ( _, Err _ ) ->
            second input



-- Helper functions


verifyResult : (a -> Bool) -> Result () a -> Result () a
verifyResult predicate result =
    case result of
        Ok a ->
            if predicate a then
                result

            else
                Err ()

        _ ->
            Err ()



-- State monad helper ################################################


type alias StateM s a =
    s -> ( s, a )


putValue : a -> StateM s a
putValue a s =
    ( s, a )


mapValue : (a -> b) -> StateM s a -> StateM s b
mapValue f sM s =
    let
        ( s2, a ) =
            sM s
    in
    ( s2, f a )


mapValue2 : (a -> b -> c) -> StateM s a -> StateM s b -> StateM s c
mapValue2 f sMA sMB s =
    let
        ( sA, a ) =
            sMA s

        ( sB, b ) =
            sMB sA
    in
    ( sB, f a b )
