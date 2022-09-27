module Exercise.TopScorersTest exposing (..)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TopScorers as TopScorers
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestHelper


rules : List Rule
rules =
    [ TopScorers.removeInsignificantPlayersMustUseFilter
    , TopScorers.resetPlayerGoalCountMustUseInsert
    , TopScorers.formatPlayersCannotUseSort
    , TopScorers.combineGamesMustUseMerge
    , TopScorers.aggregateScorersMustUseFoldlAndUpdateGoalCountForPlayer
    , TopScorers.formatPlayerMustUseMapAndWithDefault
    ]


tests : Test
tests =
    describe "Analyser"
        [ test "should report that Dict.filter must be used" <|
            \() ->
                """
module TopScorers exposing (..)

removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    Dict.toList playerGoalCounts
    |> List.filter (\\t -> Tuple.second t >= goalThreshold)
    |> Dict.fromList
            """
                    |> Review.Test.run TopScorers.removeInsignificantPlayersMustUseFilter
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (Comment "Doesn't use Dict.filter" "elm.top-scorers.use_filter" Essential Dict.empty) "removeInsignificantPlayers"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 27 } }
                        ]
        , test "should report that Dict.insert must be used" <|
            \() ->
                """
module TopScorers exposing (..)

resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    playerGoalCounts
    |> Dict.update playerName (\\ _ -> Just 0 )
            """
                    |> Review.Test.run TopScorers.resetPlayerGoalCountMustUseInsert
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (Comment "Doesn't use Dict.insert" "elm.top-scorers.use_insert" Essential Dict.empty) "resetPlayerGoalCount"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 21 } }
                        ]
        , test "should report that List.sort cannot be used" <|
            \() ->
                """
module TopScorers exposing (..)

formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    Dict.keys players |> List.sort |> String.join ", "
            """
                    |> Review.Test.run TopScorers.formatPlayersCannotUseSort
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (Comment "Uses List.sort" "elm.top-scorers.dont_use_sort" Essential Dict.empty) "List.sort"
                            |> Review.Test.atExactly { start = { row = 6, column = 26 }, end = { row = 6, column = 35 } }
                        ]
        , test "should report that Dict.merge must be used" <|
            \() ->
                """
module TopScorers exposing (..)

combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    Debug.todo "implement this function"
            """
                    |> Review.Test.run TopScorers.combineGamesMustUseMerge
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (Comment "Doesn't use Dict.merge" "elm.top-scorers.use_merge" Essential Dict.empty) "combineGames"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 13 } }
                        ]
        , test "should report that List.foldl and updateGoalCountForPlayer must be used" <|
            \() ->
                """
module TopScorers exposing (..)

aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    Debug.todo "implement this function"
            """
                    |> Review.Test.run TopScorers.aggregateScorersMustUseFoldlAndUpdateGoalCountForPlayer
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (Comment "Doesn't use List.foldl and updateGoalCountForPlayer" "elm.top-scorers.use_foldl_and_updateGoalCountForPlayer" Essential Dict.empty) "aggregateScorers"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 17 } }
                        ]
        , test "should report that Maybe.map and Maybe.withDefault must be used" <|
            \() ->
                """
module TopScorers exposing (..)

formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    Debug.todo "implement this function"
            """
                    |> Review.Test.run TopScorers.formatPlayerMustUseMapAndWithDefault
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (Comment "Doesn't use Maybe.map and Maybe.withDefault" "elm.top-scorers.use_map_and_withDefault" Essential Dict.empty) "formatPlayer"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 13 } }
                        ]
        , test "should not report anything for the exemplar" <|
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
        ]
