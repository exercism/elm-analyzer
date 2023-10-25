module Exercise.MariosMarvellousLasagnaTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.MariosMarvellousLasagna as MariosMarvellousLasagna
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "MariosMarvellousLasagnaTest"
        [ exemplar
        , otherSolutions
        , noLet
        ]


rules : List Rule
rules =
    MariosMarvellousLasagna.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module MariosMarvellousLasagna exposing (remainingTimeInMinutes)

remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes layers minutesSinceStarting =
    let
        expectedMinutesInOven =
            40

        preparationTimeInMinutes =
            2 * layers
    in
    preparationTimeInMinutes + expectedMinutesInOven - minutesSinceStarting
"""


otherSolutions : Test
otherSolutions =
    describe "other solutions that are also valid" <|
        [ test "different variable names" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module MariosMarvellousLasagna exposing (remainingTimeInMinutes)

remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes l m =
    let
        a =
            40

        b =
            2 * l
    in
    b + a - m
"""
        , test "only one declaration in let" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module MariosMarvellousLasagna exposing (remainingTimeInMinutes)

remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes layers minutesSinceStarting =
    let
        preparationTimeInMinutes =
            2 * layers
    in
    preparationTimeInMinutes + 40 - minutesSinceStarting
"""
        ]


noLet : Test
noLet =
    let
        comment =
            Comment "Doesn't use a let expression" "elm.marios-marvellous-lasagna.use_let" Essential Dict.empty
    in
    test "doesn't use a let expression" <|
        \() ->
            """
module MariosMarvellousLasagna exposing (remainingTimeInMinutes)

remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes layers minutesSinceStarting =
    (2 * layers) + 40 - minutesSinceStarting
"""
                |> Review.Test.run (MariosMarvellousLasagna.usesLet comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "remainingTimeInMinutes"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 23 } }
                    ]
