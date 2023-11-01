module Exercise.TreasureFactoryTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TreasureFactory as TreasureFactory
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "TreasureFactoryTest"
        [ exemplar
        , incorrectMakeChestSignature
        , incorrectMakeTreasureChest
        , incorrectSecureChest
        , incorrectUniqueTreasures
        ]


rules : List Rule
rules =
    TreasureFactory.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module TreasureFactory exposing (TreasureChest, getTreasure, makeChest, makeTreasureChest, secureChest, uniqueTreasures)

type TreasureChest treasure
    = TreasureChest String treasure

getTreasure : String -> TreasureChest a -> Maybe a
getTreasure passwordAttempt (TreasureChest password treasure) =
    if passwordAttempt == password then
        Just treasure

    else
        Nothing

type Chest treasure conditions
    = Chest String treasure

makeChest : String -> treasure -> Chest treasure {}
makeChest password treasure =
    Chest password treasure

secureChest : Chest treasure conditions -> Maybe (Chest treasure { conditions | securePassword : () })
secureChest (Chest password treasure) =
    if String.length password >= 8 then
        Just (Chest password treasure)

    else
        Nothing

uniqueTreasures : List (Chest treasure conditions) -> List (Chest treasure { conditions | uniqueTreasure : () })
uniqueTreasures chests =
    let
        extractTreasure (Chest _ treasure) =
            treasure

        treasures =
            List.map extractTreasure chests

        toUnique : Chest treasure conditions -> Maybe (Chest treasure { conditions | uniqueTreasure : () })
        toUnique (Chest password treasure) =
            if unique treasure treasures then
                Just (Chest password treasure)

            else
                Nothing
    in
    List.filterMap toUnique chests


unique : a -> List a -> Bool
unique element list =
    list
        |> List.filter (\\el -> el == element)
        |> List.length
        |> (==) 1

makeTreasureChest : Chest treasure { conditions | securePassword : (), uniqueTreasure : () } -> TreasureChest treasure
makeTreasureChest (Chest password treasure) =
    TreasureChest password treasure
"""


incorrectMakeChestSignature : Test
incorrectMakeChestSignature =
    let
        comment =
            Comment "makeChest signature was changed" "elm.treasure-factory.do_not_change_given_signatures" Essential Dict.empty
    in
    describe "solutions with modified makeChest signature" <|
        [ test "no signature" <|
            \() ->
                """
module TreasureFactory exposing (..)

makeChest password treasure = Chest password treasure
"""
                    |> Review.Test.run (TreasureFactory.makeChestSignatureMatchesStub comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "makeChest" ]
        , test "type argument changed" <|
            \() ->
                """
module TreasureFactory exposing (..)

makeChest : String -> treasure -> Chest treasure somethingWrong
makeChest password treasure =
    Chest password treasure
"""
                    |> Review.Test.run (TreasureFactory.makeChestSignatureMatchesStub comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "makeChest"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } }
                        ]
        , test "type alias" <|
            \() ->
                """
module TreasureFactory exposing (..)

type alias Password = String

makeChest : Password -> treasure -> Chest treasure {}
makeChest password treasure =
    Chest password treasure
"""
                    |> Review.Test.run (TreasureFactory.makeChestSignatureMatchesStub comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "makeChest"
                            |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 10 } }
                        ]
        ]


incorrectMakeTreasureChest : Test
incorrectMakeTreasureChest =
    let
        comment =
            Comment "makeTreasureChest signature was changed" "elm.treasure-factory.do_not_change_given_signatures" Essential Dict.empty
    in
    describe "solutions with modified makeTreasureChest signature" <|
        [ test "no signature" <|
            \() ->
                """
module TreasureFactory exposing (..)

makeTreasureChest (Chest password treasure) = TreasureChest password treasure
"""
                    |> Review.Test.run (TreasureFactory.makeTreasureChestSignatureMatchesStub comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "makeTreasureChest" ]
        , test "something changed" <|
            \() ->
                """
module TreasureFactory exposing (..)

makeTreasureChest : Chest treasure { conditions | securePassword : (), uniqueTreasure : Bool } -> TreasureChest treasure
makeTreasureChest (Chest password treasure) =
    TreasureChest password treasure
"""
                    |> Review.Test.run (TreasureFactory.makeTreasureChestSignatureMatchesStub comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "makeTreasureChest"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 18 } }
                        ]
        ]


incorrectSecureChest : Test
incorrectSecureChest =
    let
        comment =
            Comment "secureChest signature is incorrect" "elm.treasure-factory.incorrect_secureChest_signature" Essential Dict.empty
    in
    describe "solutions with modified secureChest signature" <|
        [ test "no signature" <|
            \() ->
                """
module TreasureFactory exposing (..)

secureChest (Chest password treasure) =
    if String.length password >= 8 then
        Just (Chest password treasure)

    else
        Nothing
"""
                    |> Review.Test.run (TreasureFactory.secureChestSignatureIsCorrect comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "secureChest" ]
        , test "something changed" <|
            \() ->
                """
module TreasureFactory exposing (..)

secureChest : Chest tReAsUrE conditions -> Maybe (Chest treasure { conditions | securePassword : () })
secureChest (Chest password treasure) =
    if String.length password >= 8 then
        Just (Chest password treasure)

    else
        Nothing
"""
                    |> Review.Test.run (TreasureFactory.secureChestSignatureIsCorrect comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "secureChest"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 12 } }
                        ]
        ]


incorrectUniqueTreasures : Test
incorrectUniqueTreasures =
    let
        comment =
            Comment "uniqueTreasures signature is incorrect" "elm.treasure-factory.incorrect_uniqueTreasures_signature" Essential Dict.empty
    in
    describe "solutions with modified uniqueTreasures signature" <|
        [ test "no signature" <|
            \() ->
                """
module TreasureFactory exposing (..)

uniqueTreasures chests = []
"""
                    |> Review.Test.run (TreasureFactory.uniqueTreasuresSignatureIsCorrect comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "uniqueTreasures" ]
        , test "something changed" <|
            \() ->
                """
module TreasureFactory exposing (..)

uniqueTreasures : List (Chest treasure conditions) -> List (Chest treasure conditions)
uniqueTreasures chests = []
"""
                    |> Review.Test.run (TreasureFactory.uniqueTreasuresSignatureIsCorrect comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "uniqueTreasures"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 16 } }
                        ]
        ]
