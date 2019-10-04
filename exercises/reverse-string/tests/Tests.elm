module Tests exposing (tests)

import Expect
import ReverseString exposing (reverseString)
import Test exposing (..)


tests : Test
tests =
    describe "Reverse String"
        [ test "an empty string" <|
            \() ->
                Expect.equal "" (reverseString "")
        , skip <|
            test "a word" <|
                \() ->
                    Expect.equal "tobor" (reverseString "robot")
        , skip <|
            test "a capitalized word" <|
                \() ->
                    Expect.equal "nemaR" (reverseString "Ramen")
        , skip <|
            test "a sentence with punctuation" <|
                \() ->
                    Expect.equal "!yrgnuh m'I" (reverseString "I'm hungry!")
        , skip <|
            test "a palindrome" <|
                \() ->
                    Expect.equal "racecar" (reverseString "racecar")
        , skip <|
            test "an even-sized word" <|
                \() ->
                    Expect.equal "reward" (reverseString "drawer")
        ]
