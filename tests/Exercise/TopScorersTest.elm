module Exercise.TopScorersTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TopScorers as TopScorers
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests =
    describe "TopScorersTest"
        [ exemplar
        , ruleTests
        ]


rules : List Rule
rules =
    TopScorers.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module TopScorers exposing (..)

import Dict exposing (Dict)
import TopScorersSupport exposing (PlayerName)


updateGoalCountForPlayer : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
updateGoalCountForPlayer playerName playerGoalCounts =
    let
        updatePlayer existingGoalCount =
            case existingGoalCount of
                Just goalCount ->
                    Just (goalCount + 1)

                Nothing ->
                    Just 1
    in
    Dict.update playerName updatePlayer playerGoalCounts


aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    List.foldl updateGoalCountForPlayer Dict.empty playerNames


removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    Dict.filter (\\_ goalCount -> goalCount >= goalThreshold) playerGoalCounts


resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    Dict.insert playerName 0 playerGoalCounts


formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    Dict.get playerName playerGoalCounts
        |> Maybe.map (\\goalCount -> playerName ++ ": " ++ String.fromInt goalCount)
        |> Maybe.withDefault (playerName ++ ": 0")


formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    Dict.toList players
        |> List.map (\\( playerName, goalCount ) -> playerName ++ ": " ++ String.fromInt goalCount)
        |> String.join ", "


combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    Dict.merge
        -- when only in game 1
        (\\playerName game1GoalCount playerGoalCounts -> Dict.insert playerName game1GoalCount playerGoalCounts)
        -- when in game 1 and game2
        (\\playerName game1GoalCount game2GoalCount playerGoalCounts -> Dict.insert playerName (game1GoalCount + game2GoalCount) playerGoalCounts)
        -- when only in game 2
        (\\playerName game2GoalCount playerGoalCounts -> Dict.insert playerName game2GoalCount playerGoalCounts)
        game1
        game2
        Dict.empty
"""


ruleTests : Test
ruleTests =
    describe "code that violates the rules"
        [ test "should report that Dict.filter must be used" <|
            \() ->
                let
                    comment =
                        Comment "Doesn't use Dict.filter" "elm.top-scorers.use_filter" Essential Dict.empty
                in
                """
module TopScorers exposing (..)

removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    Dict.toList playerGoalCounts
    |> List.filter (\\t -> Tuple.second t >= goalThreshold)
    |> Dict.fromList
            """
                    |> Review.Test.run (TopScorers.removeInsignificantPlayersMustUseFilter comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "removeInsignificantPlayers"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 27 } }
                        ]
        , test "should report that Dict.insert must be used" <|
            \() ->
                let
                    comment =
                        Comment "Doesn't use Dict.insert" "elm.top-scorers.use_insert" Essential Dict.empty
                in
                """
module TopScorers exposing (..)

resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    playerGoalCounts
    |> Dict.update playerName (\\ _ -> Just 0 )
            """
                    |> Review.Test.run (TopScorers.resetPlayerGoalCountMustUseInsert comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "resetPlayerGoalCount"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 21 } }
                        ]
        , test "should report that List.sort cannot be used" <|
            \() ->
                let
                    comment =
                        Comment "Uses List.sort" "elm.top-scorers.dont_use_sort" Essential Dict.empty
                in
                """
module TopScorers exposing (..)

formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    Dict.keys players |> List.sort |> String.join ", "
            """
                    |> Review.Test.run (TopScorers.formatPlayersCannotUseSort comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "List.sort"
                            |> Review.Test.atExactly { start = { row = 6, column = 26 }, end = { row = 6, column = 35 } }
                        ]
        , test "should report that Dict.merge must be used" <|
            \() ->
                let
                    comment =
                        Comment "Doesn't use Dict.merge" "elm.top-scorers.use_merge" Essential Dict.empty
                in
                """
module TopScorers exposing (..)

combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    game1
    |> Dict.toList
    |> List.foldr (\\(name,score) g -> Dict.update name (update score) g) game2
            """
                    |> Review.Test.run (TopScorers.combineGamesMustUseMerge comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "combineGames"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 13 } }
                        ]
        , test "should report that List.foldl and updateGoalCountForPlayer must be used" <|
            \() ->
                let
                    comment =
                        Comment "Doesn't use List.foldl and updateGoalCountForPlayer" "elm.top-scorers.use_foldl_and_updateGoalCountForPlayer" Essential Dict.empty
                in
                """
module TopScorers exposing (..)

aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    playerNames
    |> List.foldr
        (\\name acc ->
            Dict.update name
                (\\existingCount ->
                    case existingCount of
                        Just count ->
                            Just (count + 1)
                        Nothing ->
                            Just 1
                )
                acc
        )
        Dict.empty
            """
                    |> Review.Test.run (TopScorers.aggregateScorersMustUseFoldlAndUpdateGoalCountForPlayer comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "aggregateScorers"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 17 } }
                        ]
        , test "should report that Maybe.withDefault must be used" <|
            \() ->
                let
                    comment =
                        Comment "Doesn't use Maybe.withDefault" "elm.top-scorers.use_withDefault" Essential Dict.empty
                in
                """
module TopScorers exposing (..)

formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    case Dict.get playerName playerGoalCounts of
        Just n  -> playerName ++ ": " ++ String.fromInt n
        Nothing -> playerName ++ ": 0"
            """
                    |> Review.Test.run (TopScorers.formatPlayerMustUseWithDefault comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "formatPlayer"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 13 } }
                        ]
        ]
