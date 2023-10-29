module Exercise.ValentinesDayTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.ValentinesDay as ValentinesDay
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "ValentinesDayTest"
        [ exemplar
        , noCase
        ]


rules : List Rule
rules =
    ValentinesDay.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module ValentinesDay exposing (..)

type Approval
    = Yes
    | No
    | Maybe

type Cuisine
    = Korean
    | Turkish

type Genre
    = Crime
    | Horror
    | Romance
    | Thriller

type Activity
    = BoardGame
    | Chill
    | Movie Genre
    | Restaurant Cuisine

rateActivity : Activity -> Approval
rateActivity activity =
    case activity of
        Restaurant Korean ->
            Yes

        Restaurant Turkish ->
            Maybe

        Movie Romance ->
            Yes

        _ ->
            No
"""


noCase : Test
noCase =
    let
        comment =
            Comment "Doesn't use a case expression" "elm.valentines-day.use_case" Essential Dict.empty
    in
    test "doesn't use a case expression" <|
        \() ->
            """
module ValentinesDay exposing (..)

type Approval
    = Yes
    | No
    | Maybe

type Cuisine
    = Korean
    | Turkish

type Genre
    = Crime
    | Horror
    | Romance
    | Thriller

type Activity
    = BoardGame
    | Chill
    | Movie Genre
    | Restaurant Cuisine

rateActivity activity =
    if (activity == Movie Romance) || (activity == Restaurant Korean) then
        Yes

    else if activity == Restaurant Turkish then
        Maybe

    else
        No
"""
                |> Review.Test.run (ValentinesDay.usesCase comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "rateActivity" ]
