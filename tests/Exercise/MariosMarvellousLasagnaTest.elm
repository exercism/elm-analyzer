module Exercise.MariosMarvellousLasagnaTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.MariosMarvellousLasagna as MariosMarvellousLasagna
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestHelper


tests =
    describe "MariosMarvellousLasagnaTest"
        [ exemplar
        , otherSolutions
        , noLet
        ]


rules : List Rule
rules =
    [ MariosMarvellousLasagna.usesLet ]


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
    test "doesn't use a let expression" <|
        \() ->
            """
module MariosMarvellousLasagna exposing (remainingTimeInMinutes)

remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes layers minutesSinceStarting =
    (2 * layers) + 40 - minutesSinceStarting
"""
                |> Review.Test.run MariosMarvellousLasagna.usesLet
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder (Comment "Doesn't use a let expression" "elm.marios-marvellous-lasagna.use_let" Essential Dict.empty) "remainingTimeInMinutes"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 23 } }
                    ]
