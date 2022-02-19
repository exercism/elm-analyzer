module Exercise.StrainTest exposing (..)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.Strain as Strain
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestHelper


rules : List Rule
rules =
    [ Strain.doNotUseFilter ]


exampleSolution : Test
exampleSolution =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module Strain exposing (discard, keep)

import List

keep : (a -> Bool) -> List a -> List a
keep predicate list =
    List.foldr (consIf predicate) [] list


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    List.foldr (consIf (\\v -> not <| predicate v)) [] list


consIf : (a -> Bool) -> a -> List a -> List a
consIf predicate value list =
    if predicate value then
        value :: list

    else
        list
"""


otherSolutions : Test
otherSolutions =
    describe "other solutions that are also valid" <|
        [ test "using recursion" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module Strain exposing (discard, keep)

keep : (a -> Bool) -> List a -> List a
keep predicate list =
    case list of
        [] ->
            []

        head :: tail ->
            if predicate head then
                head :: keep predicate tail

            else
                keep predicate tail


discard : (a -> Bool) -> List a -> List a
discard predicate =
    keep (predicate >> not)
"""
        , test "importing List.filter and List.filterMap without using them is Ok" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module Strain exposing (discard, keep)

import List exposing (filter, filterMap)


keep : (a -> Bool) -> List a -> List a
keep predicate list =
    List.foldr (consIf predicate) [] list


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    List.foldr (consIf (\\v -> not <| predicate v)) [] list


consIf : (a -> Bool) -> a -> List a -> List a
consIf predicate value list =
    if predicate value then
        value :: list

    else
        list
"""
        ]


usingFilter : Test
usingFilter =
    describe "solutions that use filter or filterMap" <|
        [ test "using List.filter" <|
            \() ->
                """
module Strain exposing (discard, keep)

import List

keep : (a -> Bool) -> List a -> List a
keep predicate =
    List.filter predicate


discard : (a -> Bool) -> List a -> List a
discard predicate =
    List.filter (predicate >> not)
"""
                    |> Review.Test.run Strain.doNotUseFilter
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            (Comment "Uses the List module" "elm.strain.do_not_use_filter" Essential Dict.empty)
                            "List.filter"
                            |> Review.Test.atExactly { start = { row = 13, column = 5 }, end = { row = 13, column = 16 } }
                        ]
        , test "using List.filterMap" <|
            \() ->
                """
module Strain exposing (discard, keep)

import List

keep : (a -> Bool) -> List a -> List a
keep predicate =
    List.filterMap (\\a -> if predicate a then Just a else Nothing)


discard : (a -> Bool) -> List a -> List a
discard predicate =
    List.filterMap (\\a -> if predicate a then Nothing else Just a)
"""
                    |> Review.Test.run Strain.doNotUseFilter
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            (Comment "Uses the List module" "elm.strain.do_not_use_filter" Essential Dict.empty)
                            "List.filterMap"
                            |> Review.Test.atExactly { start = { row = 13, column = 5 }, end = { row = 13, column = 19 } }
                        ]
        , test "using improted List.filter and List.filterMap, only shows one error" <|
            \() ->
                """
module Strain exposing (discard, keep)

import List exposing (filter, filterMap)

keep : (a -> Bool) -> List a -> List a
keep predicate =
    filter predicate


discard : (a -> Bool) -> List a -> List a
discard predicate =
    filterMap (\\a -> if predicate a then Nothing else Just a)
"""
                    |> Review.Test.run Strain.doNotUseFilter
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            (Comment "Uses the List module" "elm.strain.do_not_use_filter" Essential Dict.empty)
                            "filter"
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 11 } }
                        ]
        , test "creating an alias somewhere else" <|
            \() ->
                """
module Strain exposing (discard, keep)

import List exposing (filter)

totallyNotFilter = 
  filter

keep : (a -> Bool) -> List a -> List a
keep predicate =
    totallyNotFilter predicate


discard : (a -> Bool) -> List a -> List a
discard predicate =
    totallyNotFilter (predicate >> not)
"""
                    |> Review.Test.run Strain.doNotUseFilter
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            (Comment "Uses the List module" "elm.strain.do_not_use_filter" Essential Dict.empty)
                            "filter"
                            |> Review.Test.atExactly { start = { row = 7, column = 3 }, end = { row = 7, column = 9 } }
                        ]
        ]
