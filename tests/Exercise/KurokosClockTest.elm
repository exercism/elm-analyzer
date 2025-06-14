module Exercise.KurokosClockTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.KurokosClock as KurokosClock
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests =
    describe "KurokosClockTest"
        [ exampleSolution
        , missingShowLocalDate
        , missingShowLocalTime
        ]


rules : List Rule
rules =
    KurokosClock.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exampleSolution : Test
exampleSolution =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module KurokosClock exposing (Locale(..), showDateTime, showLocalDate, showLocalTime)

import Time exposing (Month(..), Posix, Zone)


type Locale
    = US
    | JP


showLocalDate : Locale -> Int -> Month -> Int -> String
showLocalDate locale year month day =
    let
        yearStr =
            String.fromInt year

        monthStr =
            month |> monthToInt |> String.fromInt

        dayStr =
            String.fromInt day
    in
    case locale of
        US ->
            monthStr ++ "/" ++ dayStr ++ "/" ++ yearStr

        JP ->
            yearStr ++ "年" ++ monthStr ++ "月" ++ dayStr ++ "日"


showLocalTime : Locale -> Int -> Int -> String
showLocalTime locale hour minute =
    let
        minuteStr =
            String.fromInt minute
    in
    case locale of
        US ->
            let
                period =
                    if hour >= 12 then
                        "PM"

                    else
                        "AM"

                hour12 =
                    case modBy 12 hour of
                        0 ->
                            12

                        h ->
                            h
            in
            String.fromInt hour12 ++ ":" ++ String.padLeft 2 '0' minuteStr ++ " " ++ period

        JP ->
            String.fromInt hour ++ "時" ++ minuteStr ++ "分"


showDateTime : Locale -> Zone -> Posix -> String
showDateTime locale zone posix =
    let
        year =
            Time.toYear zone posix

        month =
            Time.toMonth zone posix

        day =
            Time.toDay zone posix

        hour =
            Time.toHour zone posix

        minute =
            Time.toMinute zone posix
    in
    case locale of
        US ->
            showLocalDate locale year month day ++ " " ++ showLocalTime locale hour minute

        JP ->
            showLocalDate locale year month day ++ showLocalTime locale hour minute


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
"""


missingShowLocalDate : Test
missingShowLocalDate =
    let
        comment =
            Comment "elm.kurokos-clock.use_showLocalDate_and_showLocalTime" Essential Dict.empty
    in
    test "not using showLocalDate" <|
        \() ->
            """
module KurokosClock exposing (Locale(..), showDateTime, showLocalDate, showLocalTime)

import Time exposing (Month(..), Posix, Zone)

type Locale
    = US
    | JP

showLocalDate : Locale -> Int -> Month -> Int -> String
showLocalDate locale year month day =
    let
        yearStr =
            String.fromInt year

        monthStr =
            month |> monthToInt |> String.fromInt

        dayStr =
            String.fromInt day
    in
    case locale of
        US ->
            monthStr ++ "/" ++ dayStr ++ "/" ++ yearStr

        JP ->
            yearStr ++ "年" ++ monthStr ++ "月" ++ dayStr ++ "日"

showLocalTime : Locale -> Int -> Int -> String
showLocalTime locale hour minute =
    let
        minuteStr =
            String.fromInt minute
    in
    case locale of
        US ->
            let
                period =
                    if hour >= 12 then
                        "PM"

                    else
                        "AM"

                hour12 =
                    case modBy 12 hour of
                        0 ->
                            12

                        h ->
                            h
            in
            String.fromInt hour12 ++ ":" ++ String.padLeft 2 '0' minuteStr ++ " " ++ period

        JP ->
            String.fromInt hour ++ "時" ++ minuteStr ++ "分"

showDateTime : Locale -> Zone -> Posix -> String
showDateTime locale zone posix =
    let
        year =
            Time.toYear zone posix

        month =
            Time.toMonth zone posix

        day =
            Time.toDay zone posix

        hour =
            Time.toHour zone posix

        minute =
            Time.toMinute zone posix

        monthStr =
            month |> monthToInt |> String.fromInt
    in
    case locale of
        US ->
            monthStr ++ "/" ++ String.fromInt day ++ "/" ++ String.fromInt year ++ " " ++ showLocalTime locale hour minute

        JP ->
            String.fromInt year ++ "年" ++ monthStr ++ "月" ++ String.fromInt day ++ "日" ++ showLocalTime locale hour minute

monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12
"""
                |> Review.Test.run (KurokosClock.usesShowLocalDateAndShowLocalTime comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder
                        comment
                        "showDateTime"
                        |> Review.Test.atExactly { start = { row = 59, column = 1 }, end = { row = 59, column = 13 } }
                    ]


missingShowLocalTime : Test
missingShowLocalTime =
    let
        comment =
            Comment "elm.kurokos-clock.use_showLocalDate_and_showLocalTime" Essential Dict.empty
    in
    test "not using showLocalTime" <|
        \() ->
            """
module KurokosClock exposing (Locale(..), showDateTime, showLocalDate, showLocalTime)

import Time exposing (Month(..), Posix, Zone)

type Locale
    = US
    | JP

showLocalDate : Locale -> Int -> Month -> Int -> String
showLocalDate locale year month day =
    let
        yearStr =
            String.fromInt year

        monthStr =
            month |> monthToInt |> String.fromInt

        dayStr =
            String.fromInt day
    in
    case locale of
        US ->
            monthStr ++ "/" ++ dayStr ++ "/" ++ yearStr

        JP ->
            yearStr ++ "年" ++ monthStr ++ "月" ++ dayStr ++ "日"

showLocalTime : Locale -> Int -> Int -> String
showLocalTime locale hour minute =
    let
        minuteStr =
            String.fromInt minute
    in
    case locale of
        US ->
            let
                period =
                    if hour >= 12 then
                        "PM"

                    else
                        "AM"

                hour12 =
                    case modBy 12 hour of
                        0 ->
                            12

                        h ->
                            h
            in
            String.fromInt hour12 ++ ":" ++ String.padLeft 2 '0' minuteStr ++ " " ++ period

        JP ->
            String.fromInt hour ++ "時" ++ minuteStr ++ "分"

showDateTime : Locale -> Zone -> Posix -> String
showDateTime locale zone posix =
    let
        year =
            Time.toYear zone posix

        month =
            Time.toMonth zone posix

        day =
            Time.toDay zone posix

        hour =
            Time.toHour zone posix

        minute =
            Time.toMinute zone posix

        minuteStr =
            String.fromInt minute

        period =
            if hour >= 12 then
                "PM"

            else
                "AM"

        hour12 =
            case modBy 12 hour of
                0 ->
                    12

                h ->
                    h
    in
    case locale of
        US ->
            showLocalDate locale year month day ++ " " ++ String.fromInt hour12 ++ ":" ++ String.padLeft 2 '0' minuteStr ++ " " ++ period

        JP ->
            showLocalDate locale year month day ++ String.fromInt hour ++ "時" ++ minuteStr ++ "分"

monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12
"""
                |> Review.Test.run (KurokosClock.usesShowLocalDateAndShowLocalTime comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder
                        comment
                        "showDateTime"
                        |> Review.Test.atExactly { start = { row = 59, column = 1 }, end = { row = 59, column = 13 } }
                    ]
