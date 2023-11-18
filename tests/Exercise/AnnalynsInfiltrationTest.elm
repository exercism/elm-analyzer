module Exercise.AnnalynsInfiltrationTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.AnnalynsInfiltration as AnnalynsInfiltration
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "AnnalynsInfiltrationTest"
        [ exemplar
        , otherSolutions
        , canFastAttackNoNot
        , canSpyNoOr
        , canSignalPrisonerNoBoolOp
        , canFreePrisonerNoBoolOp
        ]


rules : List Rule
rules =
    AnnalynsInfiltration.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module AnnalynsInfiltration exposing (canFastAttack, canFreePrisoner, canSignalPrisoner, canSpy, stealthAttackDamage)

canFastAttack : Bool -> Bool
canFastAttack knightIsAwake =
    not knightIsAwake

canSpy : Bool -> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    knightIsAwake || archerIsAwake || prisonerIsAwake

canSignalPrisoner : Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    not archerIsAwake && prisonerIsAwake

canFreePrisoner : Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    not knightIsAwake && not archerIsAwake && prisonerIsAwake || not archerIsAwake && petDogIsPresent

stealthAttackDamage : Bool -> Int
stealthAttackDamage annalynIsDetected =
    if annalynIsDetected then
        7

    else
        12
"""


otherSolutions : Test
otherSolutions =
    describe "other solutions that are less obvious but valid" <|
        [ test "canSignalPrisoner without &&" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module AnnalynsInfiltration exposing (..)

canFastAttack : Bool -> Bool
canFastAttack knightIsAwake =
    not knightIsAwake

canSpy : Bool -> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    knightIsAwake || archerIsAwake || prisonerIsAwake

canSignalPrisoner : Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    not (archerIsAwake || not prisonerIsAwake)

canFreePrisoner : Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    not knightIsAwake && not archerIsAwake && prisonerIsAwake || not archerIsAwake && petDogIsPresent
"""
        , test "canFreePrisoner with no ||" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module AnnalynsInfiltration exposing (..)

canFastAttack : Bool -> Bool
canFastAttack knightIsAwake =
    not knightIsAwake

canSpy : Bool -> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    knightIsAwake || archerIsAwake || prisonerIsAwake

canSignalPrisoner : Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    not archerIsAwake && prisonerIsAwake

canFreePrisoner : Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    not
        (not (not knightIsAwake && not archerIsAwake && prisonerIsAwake)
            && not (not archerIsAwake && petDogIsPresent)
        )
"""
        ]


canFastAttackNoNot : Test
canFastAttackNoNot =
    let
        comment =
            Comment "canFastAttack doesn't use not" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty
    in
    test "canFastAttack doesn't use not" <|
        \() ->
            """
module AnnalynsInfiltration exposing (..)

canFastAttack : Bool -> Bool
canFastAttack knightIsAwake =
    if knightIsAwake then False else True
"""
                |> Review.Test.run (AnnalynsInfiltration.canFastAttackUsesNot comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "canFastAttack"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 14 } }
                    ]


canSpyNoOr : Test
canSpyNoOr =
    let
        comment =
            Comment "canSpy doesn't use ||" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty
    in
    test "canSpy doesn't use ||" <|
        \() ->
            """
module AnnalynsInfiltration exposing (..)

canSpy : Bool -> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    if knightIsAwake then True else (if archerIsAwake then True else prisonerIsAwake)
"""
                |> Review.Test.run (AnnalynsInfiltration.canSpyUsesOr comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "canSpy"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 7 } }
                    ]


canSignalPrisonerNoBoolOp : Test
canSignalPrisonerNoBoolOp =
    let
        comment =
            Comment "canSignalPrisoner doesn't use boolean operators" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty
    in
    test "canSignalPrisoner doesn't use boolean operators" <|
        \() ->
            """
module AnnalynsInfiltration exposing (..)

canSignalPrisoner : Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    case (archerIsAwake, prisonerIsAwake) of
      (False, True) -> True
      _ -> False
"""
                |> Review.Test.run (AnnalynsInfiltration.canSignalPrisonerUsesBooleanOperators comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "canSignalPrisoner"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 18 } }
                    ]


canFreePrisonerNoBoolOp : Test
canFreePrisonerNoBoolOp =
    let
        comment =
            Comment "canFreePrisoner doesn't use boolean operators" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty
    in
    test "canFreePrisoner doesn't use boolean operators" <|
        \() ->
            """
module AnnalynsInfiltration exposing (..)

canFreePrisoner : Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    case ( ( knightIsAwake, archerIsAwake ), ( prisonerIsAwake, petDogIsPresent ) ) of
        ( ( _, False ), ( _, True ) ) ->
            True

        ( ( False, False ), ( True, _ ) ) ->
            True

        _ ->
            False

"""
                |> Review.Test.run (AnnalynsInfiltration.canFreePrisonerUsesBooleanOperators comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "canFreePrisoner"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 16 } }
                    ]
