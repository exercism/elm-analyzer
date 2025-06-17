module Exercise.SecretsTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.Secrets as Secrets
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests =
    describe "SecretsTest"
        [ exemplar
        , missingHelperFunctions
        , usingHelperFunctionsIndirectly
        ]


rules : List Rule
rules =
    Secrets.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for correct implementation using all helper functions" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module Secrets exposing (clearBits, decrypt, flipBits, setBits, shiftBack)

import Bitwise


shiftBack amount value =
    Bitwise.shiftRightZfBy amount value


setBits mask value =
    Bitwise.or mask value


flipBits mask value =
    Bitwise.xor mask value


clearBits mask value =
    mask
        |> Bitwise.complement
        |> Bitwise.and value


decrypt secret =
    secret
        |> setBits 1996
        |> flipBits 2009
        |> shiftBack 5
        |> clearBits 17
"""


missingHelperFunctions : Test
missingHelperFunctions =
    let
        comment =
            Comment "elm.secrets.use_all_helper_functions" Essential Dict.empty
    in
    describe "solutions missing helper functions" <|
        [ test "missing shiftBack function" <|
            \() ->
                """
module Secrets exposing (decrypt, shiftBack, setBits, flipBits, clearBits)

import Bitwise
shiftBack = Bitwise.shiftRightZfBy
setBits = Bitwise.or
flipBits = Bitwise.xor

clearBits mask value =
    mask
        |> Bitwise.complement
        |> Bitwise.and value

decrypt : Int -> Int
decrypt value =
    value
        |> setBits 1996
        |> flipBits 2009
        |> Bitwise.shiftRightZfBy 5
        |> clearBits 17
"""
                    |> Review.Test.run (Secrets.usesAllHelperFunctions comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "decrypt"
                            |> Review.Test.atExactly { start = { row = 15, column = 1 }, end = { row = 15, column = 8 } }
                        ]
        , test "missing setBits function" <|
            \() ->
                """
module Secrets exposing (decrypt, shiftBack, setBits, flipBits, clearBits)

import Bitwise
shiftBack = Bitwise.shiftRightZfBy
setBits = Bitwise.or
flipBits = Bitwise.xor

clearBits mask value =
    mask
        |> Bitwise.complement
        |> Bitwise.and value

decrypt secret =
    secret
        |> Bitwise.or 1996
        |> flipBits 2009
        |> shiftBack 5
        |> clearBits 17
"""
                    |> Review.Test.run (Secrets.usesAllHelperFunctions comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "decrypt"
                            |> Review.Test.atExactly { start = { row = 14, column = 1 }, end = { row = 14, column = 8 } }
                        ]
        , test "missing flipBits function" <|
            \() ->
                """
module Secrets exposing (decrypt, shiftBack, setBits, flipBits, clearBits)

import Bitwise
shiftBack = Bitwise.shiftRightZfBy
setBits = Bitwise.or
flipBits = Bitwise.xor

clearBits mask value =
    mask
        |> Bitwise.complement
        |> Bitwise.and value

decrypt secret =
    secret
        |> setBits 1996
        |> Bitwise.xor 2009
        |> shiftBack 5
        |> clearBits 17
"""
                    |> Review.Test.run (Secrets.usesAllHelperFunctions comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "decrypt"
                            |> Review.Test.atExactly { start = { row = 14, column = 1 }, end = { row = 14, column = 8 } }
                        ]
        , test "missing clearBits function" <|
            \() ->
                """
module Secrets exposing (decrypt, shiftBack, setBits, flipBits, clearBits)

import Bitwise
shiftBack = Bitwise.shiftRightZfBy
setBits = Bitwise.or
flipBits = Bitwise.xor

clearBits mask value =
    mask
        |> Bitwise.complement
        |> Bitwise.and value

decrypt secret =
    secret
        |> setBits 1996
        |> Bitwise.xor 2009
        |> shiftBack 5
        |> Bitwise.and (Bitwise.complement 17)
"""
                    |> Review.Test.run (Secrets.usesAllHelperFunctions comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "decrypt"
                            |> Review.Test.atExactly { start = { row = 14, column = 1 }, end = { row = 14, column = 8 } }
                        ]
        ]


usingHelperFunctionsIndirectly : Test
usingHelperFunctionsIndirectly =
    test "should pass when helper functions are used indirectly through other functions" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module Secrets exposing (clearBits, decrypt, flipBits, setBits, shiftBack)

import Bitwise
shiftBack = Bitwise.shiftRightZfBy
setBits = Bitwise.or
flipBits = Bitwise.xor

clearBits mask value =
    mask
        |> Bitwise.complement
        |> Bitwise.and value

helperFunction : Int -> Int
helperFunction value =
    value
        |> setBits 1996
        |> flipBits 2009


decrypt : Int -> Int
decrypt value =
    value
        |> helperFunction
        |> shiftBack 5
        |> clearBits 17
"""
