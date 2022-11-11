module Exercise.BlorkemonCardsTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.BlorkemonCards as BlorkemonCards
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests =
    describe "BlorkemonCardsTest"
        [ exemplar
        , otherSolutions
        , noMax
        , noSortBy
        , noCompareShinyPower
        , noCase
        ]


rules : List Rule
rules =
    BlorkemonCards.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module BlorkemonCards exposing
    ( Card
    , compareShinyPower
    , expectedWinner
    , isMorePowerful
    , maxPower
    , sortByCoolness
    , sortByMonsterName
    )

type alias Card =
    { monster : String, power : Int, shiny : Bool }


isMorePowerful : Card -> Card -> Bool
isMorePowerful card1 card2 =
    card1.power > card2.power


maxPower : Card -> Card -> Int
maxPower card1 card2 =
    max card1.power card2.power


sortByMonsterName : List Card -> List Card
sortByMonsterName =
    List.sortBy .monster


sortByCoolness : List Card -> List Card
sortByCoolness =
    let
        coolness { power, shiny } =
            ( negate (shinyValue shiny), negate power )
    in
    List.sortBy coolness


compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    compare ( card1.power, shinyValue card1.shiny ) ( card2.power, shinyValue card2.shiny )


expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        EQ ->
            "too close to call"

        LT ->
            card2.monster

        GT ->
            card1.monster


shinyValue : Bool -> Int
shinyValue shiny =
    if shiny then
        1

    else
        0
"""


otherSolutions : Test
otherSolutions =
    describe "other solutions that are also valid" <|
        [ test "different implementation of sortByCoolness and compareShinyPower" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module BlorkemonCards exposing (..)

type alias Card =
    { monster : String, power : Int, shiny : Bool }

maxPower : Card -> Card -> Int
maxPower card1 card2 =
    max card1.power card2.power

sortByMonsterName : List Card -> List Card
sortByMonsterName =
    List.sortBy .monster

sortByCoolness : List Card -> List Card
sortByCoolness =
    List.sortWith
        (\\c1 c2 ->
            case (c1.shiny, c2.shiny) of
                (True, False) -> LT
                (False, True) -> GT
                _ -> compare c2.power c1.power
        )

compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    case compare card1.power card2.power of
        EQ ->
            case (card1.shiny, card2.shiny) of
                (True, False) -> GT
                (False, True) -> LT
                _ -> EQ
        byPower -> byPower

expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        GT -> card1.monster
        LT -> card2.monster
        EQ -> "too close to call"
"""
        ]


noMax : Test
noMax =
    let
        comment =
            Comment "maxPower doesn't use max" "elm.blorkemon-cards.use_max" Essential Dict.empty
    in
    test "maxPower doesn't use max" <|
        \() ->
            """
module BlorkemonCards exposing (..)

maxPower card1 card2 =
    if isMorePowerful card1 card2
    then card1.power
    else card2.power
"""
                |> Review.Test.run (BlorkemonCards.maxPowerUsesMax comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "maxPower" ]


noSortBy : Test
noSortBy =
    let
        comment =
            Comment "sortByMonsterName doesn't use List.sortBy" "elm.blorkemon-cards.use_sort_by" Essential Dict.empty
    in
    test "sortByMonsterName doesn't use List.sortBy" <|
        \() ->
            """
module BlorkemonCards exposing (..)

sortByMonsterName =
    List.sortWith
        (\\c1 c2 ->
            if c1.name < c2.name then
                LT
            else if  c1.name == c2.name then
                EQ
            else
                GT
        )
"""
                |> Review.Test.run (BlorkemonCards.sortByMonsterNameUsesSortBy comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "sortByMonsterName"
                    ]


noCompareShinyPower : Test
noCompareShinyPower =
    let
        comment =
            Comment "expectedWinner doesn't use compareShinyPower" "elm.blorkemon-cards.use_shiny_power" Essential Dict.empty
    in
    test "expectedWinner doesn't use compareShinyPower" <|
        \() ->
            """
module BlorkemonCards exposing (..)

expectedWinner card1 card2 =
    case compare ( card1.power, shinyValue card1.shiny ) ( card2.power, shinyValue card2.shiny ) of
        EQ ->
            "too close to call"

        LT ->
            card2.monster

        GT ->
            card1.monster
"""
                |> Review.Test.run (BlorkemonCards.expectedWinnerUsesCompareShinyPower comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "expectedWinner"
                    ]


noCase : Test
noCase =
    let
        comment =
            Comment "Doesn't use a case expression" "elm.blorkemon-cards.use_case" Essential Dict.empty
    in
    test "expectedWinner doesn't use a case expression" <|
        \() ->
            """
module BlorkemonCards exposing (..)

expectedWinner card1 card2 =
    let order = compare ( card1.power, shinyValue card1.shiny ) ( card2.power, shinyValue card2.shiny )
    in
        if order == EQ then
            "too close to call"
        else if order == LT then
            card2.monster
        else
            card1.monster
"""
                |> Review.Test.run (BlorkemonCards.expectedWinnerUsesCase comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "expectedWinner"
                    ]
