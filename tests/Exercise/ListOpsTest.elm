module Exercise.ListOpsTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.ListOps as ListOps
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "ListOpsTest"
        [ exampleSolution
        , usingList
        ]


rules : List Rule
rules =
    ListOps.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exampleSolution : Test
exampleSolution =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module ListOps exposing
    ( append
    , concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , reverse
    )

length : List a -> Int
length list =
    foldl (\\_ acc -> 1 + acc) 0 list


reverse : List a -> List a
reverse list =
    foldl (::) [] list

foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc list =
    case list of
        [] ->
            acc

        head :: tail ->
            foldl f (f head acc) tail

foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    case list of
        [] ->
            acc

        head :: tail ->
            f head (foldr f acc tail)

map : (a -> b) -> List a -> List b
map f list =
    foldr (\\x acc -> f x :: acc) [] list

filter : (a -> Bool) -> List a -> List a
filter f list =
    foldr
        (\\x acc ->
            if f x then
                x :: acc

            else
                acc
        )
        []
        list

append : List a -> List a -> List a
append xs ys =
    foldr (::) ys xs


concat : List (List a) -> List a
concat list =
    foldr append [] list
"""


usingList : Test
usingList =
    let
        comment =
            Comment "elm.list-ops.do_not_use_list" Essential Dict.empty
    in
    describe "solutions that use the List function" <|
        [ test "using List directy" <|
            \() ->
                """
module ListOps exposing (..)

length : List a -> Int
length = List.length

reverse : List a -> List a
reverse = List.reverse
"""
                    |> Review.Test.run (ListOps.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "List.length" ]
        , test "using List with an alias" <|
            \() ->
                """
module ListOps exposing (..)
import List as TotallyNotList

length : List a -> Int
length = TotallyNotList.length

reverse : List a -> List a
reverse = TotallyNotList.reverse
"""
                    |> Review.Test.run (ListOps.doNotUseListModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "TotallyNotList.length" ]
        ]
