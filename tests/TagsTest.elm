module TagsTest exposing (tests)

import Review.Test
import Tags
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "TagsTest tests"
        [ commonTags
        , expressionTags
        ]


commonTags : Test
commonTags =
    let
        data =
            """
[
    "paradigm:functional",
    "technique:immutability",
    "uses:module"
]
"""
    in
    describe "should always return the initialTags"
        [ test "for an empty module" <|
            \() ->
                "module A exposing (..)"
                    |> Review.Test.run Tags.commonTagsRule
                    |> Review.Test.expectDataExtract data
        , test "for an non-empty module" <|
            \() ->
                """
module TwoFer exposing (twoFer)

twoFer : Maybe String -> String
twoFer name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."
"""
                    |> Review.Test.run Tags.commonTagsRule
                    |> Review.Test.expectDataExtract data
        ]


expressionTags : Test
expressionTags =
    let
        commonStart =
            """
module A exposing (..)

import Set
import Dict
import Random
import Regex
import Debug
     """

        expectData function data =
            commonStart
                ++ function
                |> Review.Test.run Tags.expressionTagsRule
                |> Review.Test.expectDataExtract data
    in
    describe "matching expressions"
        [ test "using Set function" <|
            \() ->
                expectData "f = Set.empty"
                    "[ \"construct:set\", \"technique:immutable-collection\", \"technique:sorted-collection\" ]"
        , test "using Dict function" <|
            \() ->
                expectData "f = Dict.empty"
                    "[ \"construct:dictionary\", \"technique:immutable-collection\", \"technique:sorted-collection\" ]"
        , test "using Random function" <|
            \() ->
                expectData "f = Random.constant"
                    "[ \"technique:randomness\" ]"
        , test "using Regex function" <|
            \() ->
                expectData "f = Regex.fromString"
                    "[ \"technique:regular-expression\" ]"
        , test "using inline +" <|
            \() ->
                expectData "f a b = a + b"
                    "[ \"construct:add\" ]"
        , test "using prefix +" <|
            \() ->
                expectData "f = (+)"
                    "[ \"construct:add\" ]"
        , test "using inline -" <|
            \() ->
                expectData "f a b = a - b"
                    "[ \"construct:subtract\" ]"
        , test "using prefix -" <|
            \() ->
                expectData "f = (-)"
                    "[ \"construct:subtract\" ]"
        , test "using inline *" <|
            \() ->
                expectData "f a b = a * b"
                    "[ \"construct:multiply\" ]"
        , test "using prefix *" <|
            \() ->
                expectData "f = (*)"
                    "[ \"construct:multiply\" ]"
        , test "using inline /" <|
            \() ->
                expectData "f a b = a / b"
                    "[ \"construct:divide\" ]"
        , test "using prefix /" <|
            \() ->
                expectData "f = (/)"
                    "[ \"construct:divide\" ]"
        , test "using inline //" <|
            \() ->
                expectData "f a b = a // b"
                    "[ \"construct:divide\" ]"
        , test "using prefix //" <|
            \() ->
                expectData "f = (//)"
                    "[ \"construct:divide\" ]"
        , test "using inline ==" <|
            \() ->
                expectData "f a b = a == b"
                    "[ \"construct:boolean\", \"construct:equality\", \"technique:equality-comparison\" ]"
        , test "using prefix ==" <|
            \() ->
                expectData "f = (==)"
                    "[ \"construct:boolean\", \"construct:equality\", \"technique:equality-comparison\" ]"
        , test "using inline /=" <|
            \() ->
                expectData "f a b = a /= b"
                    "[ \"construct:boolean\", \"construct:inequality\", \"technique:equality-comparison\" ]"
        , test "using ()" <|
            \() ->
                expectData "f = ()"
                    "[ \"uses:unit\" ]"
        , test "using float" <|
            \() ->
                expectData "f = 3.14"
                    "[ \"construct:float\", \"construct:floating-point-number\" ]"
        , test "using float scientific notation" <|
            \() ->
                expectData "f = 314e-2"
                    "[ \"construct:float\", \"construct:floating-point-number\" ]"
        , test "using int" <|
            \() ->
                expectData "f = 42"
                    "[ \"construct:int\", \"construct:integral-number\" ]"
        , test "using int with hex notation" <|
            \() ->
                expectData "f = 0x42"
                    "[ \"construct:hexadecimal-number\", \"construct:int\", \"construct:integral-number\" ]"
        , test "using negation and int" <|
            \() ->
                expectData "f = -42"
                    "[ \"construct:int\", \"construct:integral-number\", \"construct:unary-minus\" ]"
        , test "using a string literal" <|
            \() ->
                expectData "f = \"hello\""
                    "[ \"construct:string\" ]"
        , test "using mutiline string literal" <|
            \() ->
                expectData "f = \"\"\"\nhello\nworld\"\"\""
                    "[ \"construct:multiline-string\", \"construct:string\" ]"
        , test "using lambda" <|
            \() ->
                expectData "f x = (\\_ -> x)"
                    "[ \"construct:lambda\" ]"
        , test "using if block" <|
            \() ->
                expectData "f x y z = if x then y else z"
                    "[ \"construct:boolean\", \"construct:if\" ]"
        , test "let block without proper destructuring" <|
            \() ->
                expectData "f x y = let z = y in z"
                    "[]"
        , test "let block with tuple destructuring" <|
            \() ->
                expectData "f y = let (a, b) = y in a"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with record destructuring" <|
            \() ->
                expectData "f y = let {a, b} = y in a"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with uncons destructuring" <|
            \() ->
                expectData "f y = let a :: b = y in a"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with named destructuring" <|
            \() ->
                expectData "f y = let Thing a = y in a"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with nested destructuring" <|
            \() ->
                expectData "f y = let (Thing a) = y in a"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with a function with record destructuring" <|
            \() ->
                expectData "f y = let f2 { a } = a in f2 y"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with a function with tuple destructuring" <|
            \() ->
                expectData "f y = let f2 (a, b) = a in f2 y"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with a function with uncons destructuring" <|
            \() ->
                expectData "f y = let f2 (a :: b) = a in f2 y"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        , test "let block with a function with named destructuring" <|
            \() ->
                expectData "f y = let f2 (Thing a) = a in f2 y"
                    "[ \"construct:pattern-matching\", \"uses:destructure\"]"
        ]
