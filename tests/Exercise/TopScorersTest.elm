module Exercise.TopScorersTest exposing (..)

-- import Expect exposing (Expectation)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TopScorers as TopScorers
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, test)
import TestHelper


rules : List Rule
rules =
    [ TopScorers.removeInsignificantPlayersMustUseFilter ]


removeInsignificantPlayersMustUseFilter : Test
removeInsignificantPlayersMustUseFilter =
    test "should report that Dict.filter must be used" <|
        \() ->
            """
module TopScorers exposing (..)

removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    Debug.todo "implement this function"
            """
                |> Review.Test.run TopScorers.removeInsignificantPlayersMustUseFilter
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder (Comment "Uses the Dict module" "elm.top-scorers.use_filter" Essential Dict.empty) "removeInsignificantPlayers"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 27 } }
                    ]


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
