module Exercise.BirdCountTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.BirdCount as BirdCount
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "BirdCountTest"
        [ exemplar
        , usingList
        ]


rules : List Rule
rules =
    BirdCount.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module BirdCount exposing (busyDays, hasDayWithoutBirds, incrementDayCount, today, total)


today : List Int -> Maybe Int
today counts =
    case counts of
        [] ->
            Nothing

        head :: _ ->
            Just head


incrementDayCount : List Int -> List Int
incrementDayCount counts =
    case counts of
        [] ->
            [ 1 ]

        head :: tail ->
            (head + 1) :: tail


hasDayWithoutBirds : List Int -> Bool
hasDayWithoutBirds counts =
    case counts of
        [] ->
            False

        0 :: _ ->
            True

        _ :: tail ->
            hasDayWithoutBirds tail


total : List Int -> Int
total counts =
    case counts of
        [] ->
            0

        head :: tail ->
            head + total tail


busyDays : List Int -> Int
busyDays counts =
    case counts of
        [] ->
            0

        head :: tail ->
            if head >= 5 then
                1 + busyDays tail

            else
                busyDays tail
"""


usingList : Test
usingList =
    let
        comment =
            Comment "elm.bird-count.do_not_use_list" Essential Dict.empty
    in
    describe "solutions that use the List function" <|
        [ test "using List.head for today" <|
            \() ->
                """
module BirdCount exposing (today)

today : List Int -> Maybe Int
today = List.head
"""
                    |> Review.Test.run (BirdCount.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "List.head" ]
        , test "using List.any for hasDayWithoutBirds" <|
            \() ->
                """
module BirdCount exposing (hasDayWithoutBirds)

hasDayWithoutBirds : List Int -> Bool
hasDayWithoutBirds = List.any ((==) 0)
"""
                    |> Review.Test.run (BirdCount.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "List.any" ]
        , test "using List.sum for total" <|
            \() ->
                """
module BirdCount exposing (total)

total : List Int -> Int
total = List.sum
"""
                    |> Review.Test.run (BirdCount.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "List.sum" ]
        , test "using List.filter and List.length for busyDays" <|
            \() ->
                """
module BirdCount exposing (busyDays)

busyDays : List Int -> Int
busyDays = List.filter ((>=) 5) >> List.length
"""
                    |> Review.Test.run (BirdCount.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "List.filter" ]
        , test "using List with an alias" <|
            \() ->
                """
module BirdCount exposing (today)
import List as L

today : List Int -> Maybe Int
today = L.head
"""
                    |> Review.Test.run (BirdCount.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "L.head" ]
        , test "using List with exposing" <|
            \() ->
                """
module BirdCount exposing (today)
import List exposing (head)

today : List Int -> Maybe Int
today = head
"""
                    |> Review.Test.run (BirdCount.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "head"
                            |> Review.Test.atExactly { start = { row = 6, column = 9 }, end = { row = 6, column = 13 } }
                        ]
        ]
