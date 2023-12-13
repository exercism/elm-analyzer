module TagsTest exposing (tests)

import Expect exposing (Expectation)
import Review.Test
import Tags
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "TagsTest tests"
        [ commonTags
        , commentsTags
        , typesTags
        , expressionTypeTags
        , expressionTags
        , destructuringTags
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


commentsTags : Test
commentsTags =
    describe "comments"
        [ test "module documentation" <|
            \() ->
                """
module A exposing (..)
{-| This a module documentation blurb
-}

f x = x
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:comment\", \"construct:documentation\" ]"
        , test "top level comment" <|
            \() ->
                """
module A exposing (..)

-- this is a top level comment
f x = x
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:comment\" ]"
        , test "internal comment" <|
            \() ->
                """
module A exposing (..)

f x =
    -- this is an internal comment
    x
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:comment\" ]"
        , test "function documentation" <|
            \() ->
                """
module A exposing (..)

{-| This is a function documentation comment
-}
f x = x
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:comment\", \"construct:documentation\" ]"
        ]


typesTags : Test
typesTags =
    describe "types"
        [ test "type alias" <|
            \() ->
                """
module A exposing (..)
type alias MyString = String
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"uses:type-alias\" ]"
        , test "custom type" <|
            \() ->
                """
module A exposing (..)
type MyType = MyType
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"uses:custom-type\" ]"
        , test "union type" <|
            \() ->
                """
module A exposing (..)
type MyType = A | B
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"uses:custom-type\", \"uses:union-type\" ]"
        , test "generics type" <|
            \() ->
                """
module A exposing (..)
type MyType a = MyType
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:generic-type\", \"uses:custom-type\" ]"
        , test "direct recursive type" <|
            \() ->
                """
module A exposing (..)
type MyType = A MyType
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:recursive-type\", \"uses:custom-type\" ]"
        , test "nested recursive type" <|
            \() ->
                """
module A exposing (..)
type MyType = A | B | C { c: (String, MyType) }
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:recursive-type\", \"uses:custom-type\", \"uses:union-type\" ]"
        , test "nested generic recursive type" <|
            \() ->
                """
module A exposing (..)
type MyType a x = A { c : ( String, { a | b : MyType a Int } ) }
"""
                    |> Review.Test.run Tags.expressionTagsRule
                    |> Review.Test.expectDataExtract "[ \"construct:generic-type\", \"construct:recursive-type\", \"uses:custom-type\" ]"
        ]


commonStart : String
commonStart =
    """
module A exposing (..)

import Set
import Dict
import Random
import Regex
import Debug
     """


expectData : String -> String -> Expectation
expectData function data =
    commonStart
        ++ function
        |> Review.Test.run Tags.expressionTagsRule
        |> Review.Test.expectDataExtract data


expressionTypeTags : Test
expressionTypeTags =
    describe "matching expression types"
        [ test "Type constructors " <|
            \() -> expectData "f = Just" "[ \"construct:constructor\"]"
        , test "other functions and values too general for a tag" <|
            \() -> expectData "f x = x" "[]"
        , test "ParenthesizedExpression is too general for a tag" <|
            \() -> expectData "f x = (x)" "[]"
        , test "GLSLExpression" <|
            \() -> expectData "f = [glsl| void main () {} |]" "[ \"uses:glsl\" ]"
        , test "Application" <|
            \() -> expectData "f x y = x y" "[ \"uses:function-application\" ]"
        , test "PrefixOperator" <|
            \() -> expectData "f = (^)" "[ \"uses:prefix-operator\" ]"
        , test "using ()" <|
            \() -> expectData "f = ()" "[ \"uses:unit\" ]"
        , test "using float" <|
            \() ->
                expectData "f = 3.14" "[ \"construct:float\", \"construct:floating-point-number\" ]"
        , test "using float scientific notation" <|
            \() ->
                expectData "f = 314e-2" "[ \"construct:float\", \"construct:floating-point-number\" ]"
        , test "using int" <|
            \() ->
                expectData "f = 42" "[ \"construct:int\", \"construct:integral-number\" ]"
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
                expectData "f = \"hello\"" "[ \"construct:string\" ]"
        , test "using mutiline string literal" <|
            \() ->
                expectData "f = \"\"\"\nhello\nworld\"\"\""
                    "[ \"construct:multiline-string\", \"construct:string\" ]"
        , test "using lambda" <|
            \() -> expectData "f x = (\\_ -> x)" "[ \"construct:lambda\" ]"
        , test "using if block" <|
            \() ->
                expectData "f x y z = if x then y else z"
                    "[ \"construct:boolean\", \"construct:if\" ]"
        , test "using let block" <|
            \() ->
                expectData "f x y = let z = y in z" "[ \"construct:assignment\" ]"
        , test "using char" <|
            \() -> expectData "f = 'a'" "[ \"construct:char\" ]"
        , test "using tuple" <|
            \() -> expectData "f a b = (a, b)" "[ \"construct:tuple\" ]"
        , test "using list" <|
            \() -> expectData "f a b = [a, b]" "[ \"construct:list\" ]"
        , test "using case" <|
            \() -> expectData "f a = case a of\n b -> b" "[ \"construct:pattern-matching\" ]"
        , test "using record" <|
            \() -> expectData "f b = {a = b}" "[ \"construct:record\" ]"
        , test "using record access" <|
            \() -> expectData "f rec = rec.field" "[ \"construct:record\", \"uses:record-access\" ]"
        , test "using record access function" <|
            \() ->
                expectData "f = .field"
                    "[ \"construct:record\", \"uses:record-access\", \"uses:record-access-function\" ]"
        , test "using record update" <|
            \() -> expectData "f a = {a | b = a}" "[ \"construct:record\", \"uses:record-update\" ]"
        ]


expressionTags : Test
expressionTags =
    describe "matching expressions"
        [ test "using Bitwise.and" <|
            \() ->
                expectData "f = Bitwise.and"
                    "[ \"construct:bit-manipulation\", \"construct:bitwise-and\" ]"
        , test "using Bitwise.or" <|
            \() ->
                expectData "f = Bitwise.or"
                    "[ \"construct:bit-manipulation\", \"construct:bitwise-or\" ]"
        , test "using Bitwise.xor" <|
            \() ->
                expectData "f = Bitwise.xor"
                    "[ \"construct:bit-manipulation\", \"construct:bitwise-xor\" ]"
        , test "using Bitwise.complement" <|
            \() ->
                expectData "f = Bitwise.complement"
                    "[ \"construct:bit-manipulation\", \"construct:bitwise-not\" ]"
        , test "using Bitwise.shiftLeftBy" <|
            \() ->
                expectData "f = Bitwise.shiftLeftBy"
                    "[ \"construct:bit-manipulation\", \"construct:bitwise-left-shift\" , \"technique:bit-shifting\"]"
        , test "using Bitwise.shiftRightBy" <|
            \() ->
                expectData "f = Bitwise.shiftRightBy"
                    "[ \"construct:bit-manipulation\", \"construct:bitwise-right-shift\", \"technique:bit-shifting\" ]"
        , test "using Bitwise.shiftRightZfBy" <|
            \() ->
                expectData "f = Bitwise.shiftRightZfBy"
                    "[ \"construct:bit-manipulation\", \"technique:bit-shifting\" ]"
        , test "using Array function" <|
            \() ->
                expectData "f = Array.empty"
                    "[ \"construct:array\", \"technique:immutable-collection\" ]"
        , test "using Bytes function" <|
            \() -> expectData "f = Bytes.width" "[ \"construct:byte\" ]"
        , test "using Bytes.Encode function" <|
            \() -> expectData "f = Bytes.Encode.encode" "[ \"construct:byte\" ]"
        , test "using List function" <|
            \() -> expectData "f = List.all" "[ \"construct:list\" ]"
        , test "using Set function" <|
            \() ->
                expectData "f = Set.empty"
                    "[ \"construct:set\", \"technique:immutable-collection\", \"technique:sorted-collection\" ]"
        , test "using Time function" <|
            \() -> expectData "f = Time.now" "[ \"construct:date-time\" ]"
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
                    "[ \"construct:add\" , \"uses:function-application\"]"
        , test "using prefix +" <|
            \() ->
                expectData "f = (+)"
                    "[ \"construct:add\", \"uses:prefix-operator\" ]"
        , test "using inline -" <|
            \() ->
                expectData "f a b = a - b"
                    "[ \"construct:subtract\" , \"uses:function-application\"]"
        , test "using prefix -" <|
            \() ->
                expectData "f = (-)"
                    "[ \"construct:subtract\", \"uses:prefix-operator\" ]"
        , test "using inline *" <|
            \() ->
                expectData "f a b = a * b"
                    "[ \"construct:multiply\" , \"uses:function-application\"]"
        , test "using prefix *" <|
            \() ->
                expectData "f = (*)"
                    "[ \"construct:multiply\", \"uses:prefix-operator\" ]"
        , test "using inline /" <|
            \() ->
                expectData "f a b = a / b"
                    "[ \"construct:divide\" , \"uses:function-application\"]"
        , test "using prefix /" <|
            \() ->
                expectData "f = (/)"
                    "[ \"construct:divide\", \"uses:prefix-operator\" ]"
        , test "using inline //" <|
            \() ->
                expectData "f a b = a // b"
                    "[ \"construct:divide\" , \"uses:function-application\"]"
        , test "using prefix //" <|
            \() ->
                expectData "f = (//)"
                    "[ \"construct:divide\", \"uses:prefix-operator\" ]"
        , test "using inline ==" <|
            \() ->
                expectData "f a b = a == b"
                    "[ \"construct:boolean\", \"construct:equality\", \"technique:equality-comparison\" , \"uses:function-application\"]"
        , test "using prefix ==" <|
            \() ->
                expectData "f = (==)"
                    "[ \"construct:boolean\", \"construct:equality\", \"technique:equality-comparison\", \"uses:prefix-operator\" ]"
        , test "using inline /=" <|
            \() ->
                expectData "f a b = a /= b"
                    "[ \"construct:boolean\", \"construct:inequality\", \"technique:equality-comparison\" , \"uses:function-application\"]"
        , test "using prefix /=" <|
            \() ->
                expectData "f = (/=)"
                    "[ \"construct:boolean\", \"construct:inequality\", \"technique:equality-comparison\", \"uses:prefix-operator\" ]"
        , test "using inline &&" <|
            \() ->
                expectData "f a b = a && b"
                    "[ \"construct:boolean\", \"construct:logical-and\", \"technique:boolean-logic\", \"uses:function-application\"]"
        , test "using prefix &&" <|
            \() ->
                expectData "f = (&&)"
                    "[ \"construct:boolean\", \"construct:logical-and\", \"technique:boolean-logic\", \"uses:prefix-operator\" ]"
        , test "using inline ||" <|
            \() ->
                expectData "f a b = a || b"
                    "[ \"construct:boolean\", \"construct:logical-or\", \"technique:boolean-logic\", \"uses:function-application\"]"
        , test "using prefix ||" <|
            \() ->
                expectData "f = (||)"
                    "[ \"construct:boolean\", \"construct:logical-or\", \"technique:boolean-logic\", \"uses:prefix-operator\" ]"
        , test "using not" <|
            \() ->
                expectData "f x = not x"
                    "[ \"construct:boolean\", \"construct:logical-not\", \"technique:boolean-logic\", \"uses:function-application\" ]"
        , test "using xor" <|
            \() ->
                expectData "f x y = xor x y"
                    "[ \"construct:boolean\", \"technique:boolean-logic\", \"uses:function-application\" ]"
        , test "using True" <|
            \() -> expectData "f = True" "[ \"construct:boolean\", \"construct:constructor\" ]"
        , test "using False" <|
            \() -> expectData "f = False" "[ \"construct:boolean\", \"construct:constructor\" ]"
        , test "using isNaN" <|
            \() ->
                expectData "f x = isNaN x"
                    "[ \"construct:boolean\", \"construct:float\", \"construct:floating-point-number\", \"uses:function-application\" ]"
        , test "using isInfinite" <|
            \() ->
                expectData "f x = isInfinite x"
                    "[ \"construct:boolean\", \"construct:float\", \"construct:floating-point-number\", \"uses:function-application\" ]"
        ]


destructuringTags : Test
destructuringTags =
    describe "destructuring"
        [ test "let block without proper destructuring" <|
            \() ->
                expectData "f x y = let z = y in z"
                    "[ \"construct:assignment\" ]"
        , test "let block with tuple destructuring" <|
            \() ->
                expectData "f y = let (a, b) = y in a"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with record destructuring" <|
            \() ->
                expectData "f y = let {a, b} = y in a"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with uncons destructuring" <|
            \() ->
                expectData "f y = let a :: b = y in a"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with named destructuring" <|
            \() ->
                expectData "f y = let Thing a = y in a"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with nested destructuring" <|
            \() ->
                expectData "f y = let (Thing a) = y in a"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with a function with record destructuring" <|
            \() ->
                expectData "f y = let f2 { a } = a in f2"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with a function with tuple destructuring" <|
            \() ->
                expectData "f y = let f2 (a, b) = a in b"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with a function with uncons destructuring" <|
            \() ->
                expectData "f y = let f2 (a :: b) = a in b"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "let block with a function with named destructuring" <|
            \() ->
                expectData "f y = let f2 (Thing a) = a in f2"
                    "[ \"construct:assignment\", \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "top-level function without proper destructuring" <|
            \() ->
                expectData "f x y = x" "[]"
        , test "top-level function with tuple destructuring" <|
            \() ->
                expectData "f (a, b) = a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "top-level function with record destructuring" <|
            \() ->
                expectData "f {a, b} = a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "top-level function with uncons destructuring" <|
            \() ->
                expectData "f (a :: b) = a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "top-level function with named destructuring" <|
            \() ->
                expectData "f (Thing a) = a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "top-level function with nested destructuring" <|
            \() ->
                expectData "f (Thing { a }) = a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "lambda without proper destructuring" <|
            \() ->
                expectData "f = \\x y -> x" "[\"construct:lambda\"]"
        , test "lambda with tuple destructuring" <|
            \() ->
                expectData "f = \\(a, b) -> a"
                    "[ \"construct:destructuring\", \"construct:lambda\", \"construct:pattern-matching\"]"
        , test "lambda with record destructuring" <|
            \() ->
                expectData "f = \\{a, b} -> a"
                    "[ \"construct:destructuring\", \"construct:lambda\", \"construct:pattern-matching\"]"
        , test "lambda with uncons destructuring" <|
            \() ->
                expectData "f = \\(a :: b) -> a"
                    "[ \"construct:destructuring\", \"construct:lambda\", \"construct:pattern-matching\"]"
        , test "lambda with named destructuring" <|
            \() ->
                expectData "f = \\(Thing a) -> a"
                    "[ \"construct:destructuring\", \"construct:lambda\", \"construct:pattern-matching\"]"
        , test "lambda with nested destructuring" <|
            \() ->
                expectData "f = \\(Thing { a }) -> a"
                    "[ \"construct:destructuring\", \"construct:lambda\", \"construct:pattern-matching\"]"
        , test "case without proper destructuring" <|
            \() ->
                expectData "f x = case x of\n x -> x" "[\"construct:pattern-matching\"]"
        , test "case with tuple destructuring" <|
            \() ->
                expectData "f x = case x of\n (a, b) -> a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "case with record destructuring" <|
            \() ->
                expectData "f x = case x of\n {a, b} -> a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "case with uncons destructuring" <|
            \() ->
                expectData "f x = case x of\n (a :: b) -> a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "case with named destructuring" <|
            \() ->
                expectData "f x = case x of\n (Thing a) -> a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        , test "case with nested destructuring" <|
            \() ->
                expectData "f x = case x of\n (Thing { a }) -> a"
                    "[ \"construct:destructuring\", \"construct:pattern-matching\"]"
        ]
