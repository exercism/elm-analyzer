module Exercise.BettysBikeShopTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.BettysBikeShop as BettysBikeShop
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "BettysBikeShopTest"
        [ exemplar
        , otherSolutions
        , noFunctionSignature
        , noImportString
        ]


rules : List Rule
rules =
    BettysBikeShop.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

poundsToString : Float -> String
poundsToString pounds =
    "£" ++ String.fromFloat pounds
"""


otherSolutions : Test
otherSolutions =
    describe "other solutions that are also valid" <|
        [ test "importing String.fromFloat" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String exposing (fromFloat)

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

poundsToString : Float -> String
poundsToString pounds =
    "£" ++ fromFloat pounds
"""
        , test "importing String with an alias" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String as S

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

poundsToString : Float -> String
poundsToString pounds =
    "£" ++ S.fromFloat pounds
"""
        , test "with a helper function" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

poundsToString : Float -> String
poundsToString =
    String.fromFloat >> addPoundsSymbol

addPoundsSymbol : String -> String
addPoundsSymbol =
    (++) "£"
"""
        ]


noFunctionSignature : Test
noFunctionSignature =
    let
        comment =
            Comment "elm.bettys-bike-shop.use_signature" Essential Dict.empty
    in
    describe "solutions without function signatures" <|
        [ test "no function signature on penceToPounds" <|
            \() ->
                """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String

-- penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

poundsToString : Float -> String
poundsToString pounds =
    "£" ++ String.fromFloat pounds
"""
                    |> Review.Test.run (BettysBikeShop.hasFunctionSignatures comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "penceToPounds"
                            |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 14 } }
                        ]
        , test "no function signature on poundsToString" <|
            \() ->
                """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

-- poundsToString : Float -> String
poundsToString pounds =
    "£" ++ String.fromFloat pounds
"""
                    |> Review.Test.run (BettysBikeShop.hasFunctionSignatures comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "poundsToString"
                            |> Review.Test.atExactly { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } }
                        ]
        , test "no function signature at all only emits one message" <|
            \() ->
                """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String

-- penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

-- poundsToString : Float -> String
poundsToString pounds =
    "£" ++ String.fromFloat pounds
"""
                    |> Review.Test.run (BettysBikeShop.hasFunctionSignatures comment)
                    |> Review.Test.expectErrors
                        -- location of the error is on last function found, but it won't be shown to students anyway
                        [ TestHelper.createExpectedErrorUnder comment "poundsToString"
                            |> Review.Test.atExactly { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } }
                        ]
        , test "helper function with no signature" <|
            \() ->
                """
module BettysBikeShop exposing (penceToPounds, poundsToString)

import String

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

poundsToString : Float -> String
poundsToString =
    String.fromFloat >> addPoundsSymbol

-- addPoundsSymbol : String -> String
addPoundsSymbol =
    (++) "£"
"""
                    |> Review.Test.run (BettysBikeShop.hasFunctionSignatures comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "addPoundsSymbol"
                            |> Review.Test.atExactly { start = { row = 15, column = 1 }, end = { row = 15, column = 16 } }
                        ]
        ]


noImportString : Test
noImportString =
    let
        comment =
            Comment "elm.bettys-bike-shop.import_string" Essential Dict.empty
    in
    describe "solutions that do no import String" <|
        [ test "without the import String" <|
            \() ->
                """
module BettysBikeShop exposing (penceToPounds, poundsToString)

penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100.0

poundsToString : Float -> String
poundsToString pounds =
    "£" ++ String.fromFloat pounds
"""
                    |> Review.Test.run (BettysBikeShop.importString comment)
                    |> Review.Test.expectGlobalErrors
                        [ TestHelper.createExpectedGlobalError comment ]
        ]
