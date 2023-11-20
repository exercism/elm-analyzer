module Exercise.GottaSnatchEmAllTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.GottaSnatchEmAll as GottaSnatchEmAll
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "GottaSnatchEmAllTest"
        [ exemplar
        , noSingleton
        , noSetInRemoveDuplicates
        , noDiff
        , noIntersect
        , boringCardsNoFold
        , noUnion
        , totalCardNoFold
        , noPartition
        ]


rules : List Rule
rules =
    GottaSnatchEmAll.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

newCollection : Card -> Set Card
newCollection =
    Set.singleton

addCard : Card -> Set Card -> ( Bool, Set Card )
addCard card collection =
    ( Set.member card collection, Set.insert card collection )

tradeCard : Card -> Card -> Set Card -> ( Bool, Set Card )
tradeCard yourCard theirCard collection =
    ( Set.member yourCard collection && not (Set.member theirCard collection)
    , collection |> Set.remove yourCard |> Set.insert theirCard
    )

removeDuplicates : List Card -> List Card
removeDuplicates =
    Set.fromList >> Set.toList

extraCards : Set Card -> Set Card -> Int
extraCards yourCollection theirCollection =
    Set.diff yourCollection theirCollection
        |> Set.size

boringCards : List (Set Card) -> List Card
boringCards collections =
    case collections of
        [] ->
            []

        collection :: rest ->
            List.foldl Set.intersect collection rest
                |> Set.toList

totalCards : List (Set Card) -> Int
totalCards =
    List.foldl Set.union Set.empty
        >> Set.size

splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards collection =
    let
        ( shiny, notShiny ) =
            Set.partition (String.startsWith "Shiny") collection
    in
    ( Set.toList shiny, Set.toList notShiny )
"""


noSingleton : Test
noSingleton =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.use_singleton" Actionable Dict.empty
    in
    test "newCollection doesn't use Set.singleton" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

newCollection : Card -> Set Card
newCollection card =
    Set.fromList [card]
"""
                |> Review.Test.run (GottaSnatchEmAll.usesSingleton comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "newCollection"
                        |> Review.Test.atExactly { start = { row = 10, column = 1 }, end = { row = 10, column = 14 } }
                    ]


noSetInRemoveDuplicates : Test
noSetInRemoveDuplicates =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.use_set" Essential Dict.empty
    in
    test "removeDuplicates doesn't use Set functions" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

removeDuplicates : List Card -> List Card
removeDuplicates cards =
    case List.sort cards of
        [] ->
            []

        card :: rest ->
            card :: removeDuplicates (List.filter ((/=) card) rest)
"""
                |> Review.Test.run (GottaSnatchEmAll.removeDuplicatesUsesSet comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "removeDuplicates"
                        |> Review.Test.atExactly { start = { row = 10, column = 1 }, end = { row = 10, column = 17 } }
                    ]


noDiff : Test
noDiff =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.use_diff" Essential Dict.empty
    in
    test "extraCards doesn't use Set.diff" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

extraCards : Set Card -> Set Card -> Int
extraCards yourCollection theirCollection =
    List.foldl Set.remove yourCollection (Set.toList theirCollection)
        |> Set.size
"""
                |> Review.Test.run (GottaSnatchEmAll.extraCardsUsesDiff comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "extraCards"
                        |> Review.Test.atExactly { start = { row = 10, column = 1 }, end = { row = 10, column = 11 } }
                    ]


noIntersect : Test
noIntersect =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.use_intersect" Essential Dict.empty
    in
    test "boringCards doesn't use Set.intersect" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

removeDuplicates : List Card -> List Card
removeDuplicates =
    Set.fromList >> Set.toList

boringCards : List (Set Card) -> List Card
boringCards sets =
    let
        cards =
            List.concatMap Set.toList sets
    in
    cards
        |> List.filter (\\card -> List.length (List.filter ((==) card) cards) == List.length sets)
        |> removeDuplicates
"""
                |> Review.Test.run (GottaSnatchEmAll.boringCardsUsesIntersect comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "boringCards"
                        |> Review.Test.atExactly { start = { row = 14, column = 1 }, end = { row = 14, column = 12 } }
                    ]


boringCardsNoFold : Test
boringCardsNoFold =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.boringCards_use_fold" Actionable Dict.empty
    in
    test "boringCards doesn't use a fold" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

removeDuplicates : List Card -> List Card
removeDuplicates =
    Set.fromList >> Set.toList

boringCards : List (Set Card) -> List Card
boringCards sets =
    let
        cards =
            List.concatMap Set.toList sets
    in
    cards
        |> List.filter (\\card -> List.length (List.filter ((==) card) cards) == List.length sets)
        |> removeDuplicates
"""
                |> Review.Test.run (GottaSnatchEmAll.boringCardsUsesFold comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "boringCards"
                        |> Review.Test.atExactly { start = { row = 14, column = 1 }, end = { row = 14, column = 12 } }
                    ]


noUnion : Test
noUnion =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.use_union" Essential Dict.empty
    in
    test "totalCards doesn't use Set.union" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

removeDuplicates : List Card -> List Card
removeDuplicates =
    Set.fromList >> Set.toList

totalCards : List (Set Card) -> Int
totalCards =
    List.concatMap Set.toList >> removeDuplicates >> List.length
"""
                |> Review.Test.run (GottaSnatchEmAll.totalCardsUsesUnion comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "totalCards"
                        |> Review.Test.atExactly { start = { row = 14, column = 1 }, end = { row = 14, column = 11 } }
                    ]


totalCardNoFold : Test
totalCardNoFold =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.totalCards_use_fold" Actionable Dict.empty
    in
    test "totalCards doesn't use a fold" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

removeDuplicates : List Card -> List Card
removeDuplicates =
    Set.fromList >> Set.toList

totalCards : List (Set Card) -> Int
totalCards =
    List.concatMap Set.toList >> removeDuplicates >> List.length
"""
                |> Review.Test.run (GottaSnatchEmAll.totalCardsUsesFold comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "totalCards"
                        |> Review.Test.atExactly { start = { row = 14, column = 1 }, end = { row = 14, column = 11 } }
                    ]


noPartition : Test
noPartition =
    let
        comment =
            Comment "elm.gotta-snatch-em-all.use_partition" Essential Dict.empty
    in
    test "splitShinyCards doesn't use Set.partition" <|
        \() ->
            """
module GottaSnatchEmAll exposing (..)

import Set exposing (Set)

type alias Card =
    String

splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards =
    Set.toList >> List.partition (String.startsWith "Shiny")
"""
                |> Review.Test.run (GottaSnatchEmAll.splitShinyCardsUsesPartition comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "splitShinyCards"
                        |> Review.Test.atExactly { start = { row = 10, column = 1 }, end = { row = 10, column = 16 } }
                    ]
