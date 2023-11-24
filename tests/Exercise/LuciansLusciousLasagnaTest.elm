module Exercise.LuciansLusciousLasagnaTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.LuciansLusciousLasagna as LuciansLusciousLasagna
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "LuciansLusciousLasagnaTest"
        [ exemplar
        , doesNotReuseFunction
        ]


rules : List Rule
rules =
    LuciansLusciousLasagna.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

expectedMinutesInOven =
    40

preparationTimeInMinutes layers =
    2 * layers

elapsedTimeInMinutes layers passedAlready =
    passedAlready + preparationTimeInMinutes layers
"""


doesNotReuseFunction : Test
doesNotReuseFunction =
    let
        comment =
            Comment "elm.lucians-luscious-lasagna.reuse_functions" Essential Dict.empty
    in
    test "elapsedTimeInMinutes does not use preparationTimeInMinutes" <|
        \() ->
            """
module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

expectedMinutesInOven =
    40

preparationTimeInMinutes layers =
    2 * layers

elapsedTimeInMinutes layers passedAlready =
    passedAlready + 2 * layers
"""
                |> Review.Test.run (LuciansLusciousLasagna.reuseFunctions comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "elapsedTimeInMinutes"
                        |> Review.Test.atExactly { start = { row = 10, column = 1 }, end = { row = 10, column = 21 } }
                    ]
