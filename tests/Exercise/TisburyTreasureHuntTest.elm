module Exercise.TisburyTreasureHuntTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TisburyTreasureHunt as TisburyTreasureHunt
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "TisburyTreasureHuntTest"
        [ exemplar
        , specialCaseSwapPossibleDoesntUseTupleInCase
        , treasureLocationMatchesPlaceLocationDoesntUsePlaceLocationToTreasureLocation
        , countPlaceTreasuresDoesntUseTupleSecond
        ]


rules : List Rule
rules =
    TisburyTreasureHunt.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module TisburyTreasureHunt exposing (..)

type alias TreasureLocation =
    ( Int, Char )

type alias Treasure =
    ( String, TreasureLocation )

type alias PlaceLocation =
    ( Char, Int )

type alias Place =
    ( String, PlaceLocation )

placeLocationToTreasureLocation : PlaceLocation -> TreasureLocation
placeLocationToTreasureLocation ( x, y ) =
    ( y, x )

treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation placeLocation treasureLocation =
    treasureLocation == placeLocationToTreasureLocation placeLocation

countPlaceTreasures : Place -> List Treasure -> Int
countPlaceTreasures ( _, placeLocation ) treasures =
    List.map Tuple.second treasures
        |> List.filter (treasureLocationMatchesPlaceLocation placeLocation)
        |> List.length

specialCaseSwapPossible : Treasure -> Place -> Treasure -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    case ( foundTreasure, place, desiredTreasure ) of
        ( "Amethyst Octopus", "Stormy Breakwater", "Crystal Crab" ) ->
            True

        ( "Amethyst Octopus", "Stormy Breakwater", "Glass Starfish" ) ->
            True

        ( "Vintage Pirate Hat", "Harbor Managers Office", "Model Ship in Large Bottle" ) ->
            True

        ( "Vintage Pirate Hat", "Harbor Managers Office", "Antique Glass Fishnet Float" ) ->
            True

        ( "Brass Spyglass", "Abandoned Lighthouse", _ ) ->
            True

        _ ->
            False
"""


specialCaseSwapPossibleDoesntUseTupleInCase : Test
specialCaseSwapPossibleDoesntUseTupleInCase =
    let
        comment =
            Comment "specialCaseSwapPossible doesn't use a tuple in a case" "elm.tisbury-treasure-hunt.use_tuple_in_case" Essential Dict.empty
    in
    test "specialCaseSwapPossible doesn't use a tuple in a case" <|
        \() ->
            """
module TisburyTreasureHunt exposing (..)

type alias TreasureLocation =
    ( Int, Char )

type alias Treasure =
    ( String, TreasureLocation )

type alias PlaceLocation =
    ( Char, Int )

type alias Place =
    ( String, PlaceLocation )

specialCaseSwapPossible : Treasure -> Place -> Treasure -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    ( foundTreasure, place, desiredTreasure ) == ( "Amethyst Octopus", "Stormy Breakwater", "Crystal Crab" ) ||
    ( foundTreasure, place, desiredTreasure ) == ( "Amethyst Octopus", "Stormy Breakwater", "Glass Starfish" ) ||
    ( foundTreasure, place, desiredTreasure ) ==  ( "Vintage Pirate Hat", "Harbor Managers Office", "Model Ship in Large Bottle" )  ||
    ( foundTreasure, place, desiredTreasure ) == ( "Vintage Pirate Hat", "Harbor Managers Office", "Antique Glass Fishnet Float" ) ||
    ( foundTreasure, place ) ==  ( "Brass Spyglass", "Abandoned Lighthouse")
"""
                |> Review.Test.run (TisburyTreasureHunt.specialCaseSwapPossibleShouldTupleInCase comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "specialCaseSwapPossible"
                        |> Review.Test.atExactly { start = { row = 17, column = 1 }, end = { row = 17, column = 24 } }
                    ]


treasureLocationMatchesPlaceLocationDoesntUsePlaceLocationToTreasureLocation : Test
treasureLocationMatchesPlaceLocationDoesntUsePlaceLocationToTreasureLocation =
    let
        comment =
            Comment "treasureLocationMatchesPlaceLocation doesn't use placeLocationToTreasureLocation" "elm.tisbury-treasure-hunt.use_placeLocationToTreasureLocation" Actionable Dict.empty
    in
    test "treasureLocationMatchesPlaceLocation doesn't use placeLocationToTreasureLocation" <|
        \() ->
            """
module TisburyTreasureHunt exposing (..)

type alias TreasureLocation =
    ( Int, Char )

type alias Treasure =
    ( String, TreasureLocation )

type alias PlaceLocation =
    ( Char, Int )

type alias Place =
    ( String, PlaceLocation )

treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation ( x, y ) treasureLocation =
    treasureLocation == ( y, x )
"""
                |> Review.Test.run (TisburyTreasureHunt.treasureLocationMatchesPlaceLocationUsesPlaceLocationToTreasureLocation comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "treasureLocationMatchesPlaceLocation"
                        |> Review.Test.atExactly { start = { row = 17, column = 1 }, end = { row = 17, column = 37 } }
                    ]


countPlaceTreasuresDoesntUseTupleSecond : Test
countPlaceTreasuresDoesntUseTupleSecond =
    let
        comment =
            Comment "countPlaceTreasures doesn't use Tuple.second" "elm.tisbury-treasure-hunt.use_tuple_second" Actionable Dict.empty
    in
    test "countPlaceTreasures doesn't use Tuple.second" <|
        \() ->
            """
module TisburyTreasureHunt exposing (..)

type alias TreasureLocation =
    ( Int, Char )

type alias Treasure =
    ( String, TreasureLocation )

type alias PlaceLocation =
    ( Char, Int )

type alias Place =
    ( String, PlaceLocation )

countPlaceTreasures : Place -> List Treasure -> Int
countPlaceTreasures ( _, placeLocation ) treasures =
    treasures
        |> List.filter (\\( _, x ) -> treasureLocationMatchesPlaceLocation placeLocation x)
        |> List.length
"""
                |> Review.Test.run (TisburyTreasureHunt.countPlaceTreasuresUsesTupleSecond comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "countPlaceTreasures"
                        |> Review.Test.atExactly { start = { row = 17, column = 1 }, end = { row = 17, column = 20 } }
                    ]
