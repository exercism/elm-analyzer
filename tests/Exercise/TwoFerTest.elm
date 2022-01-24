module Exercise.TwoFerTest exposing (..)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TwoFer as TwoFer
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestHelper


rules : List Rule
rules =
    [ TwoFer.hasFunctionSignature
    , TwoFer.usesWithDefault
    ]


exampleSolution : Test
exampleSolution =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module TwoFer exposing (twoFer)

twoFer : Maybe String -> String
twoFer name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."
"""


otherSolutions : Test
otherSolutions =
    describe "other solutions that are also valid" <|
        [ test "exposing Maybe.withDefault doesn't fool the rule" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module TwoFer exposing (twoFer)

import Maybe exposing (withDefault)

twoFer : Maybe String -> String
twoFer name =
    "One for "
        ++ withDefault "you" name
        ++ ", one for me."
"""
        , test "importing Maybe with an alias doesn't fool the rule" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module TwoFer exposing (twoFer)

import Maybe as Possibly

twoFer : Maybe String -> String
twoFer name =
    "One for "
        ++ Possibly.withDefault "you" name
        ++ ", one for me."
"""
        ]


noFuctionSignature : Test
noFuctionSignature =
    let
        error =
            TestHelper.createExpectedErrorUnder
                (Comment "has no signature" "elm.two-fer.use_signature" Informative Dict.empty)
                "twoFer"
    in
    describe "solutions without function signatures" <|
        [ test "no function signature" <|
            \() ->
                """
module TwoFer exposing (twoFer)

twoFer name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."
"""
                    |> Review.Test.run TwoFer.hasFunctionSignature
                    |> Review.Test.expectErrors
                        [ error
                            |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 7 } }
                        ]
        , test "commented function signature" <|
            \() ->
                """
module TwoFer exposing (twoFer)

-- twoFer : Maybe String -> String
twoFer name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."
"""
                    |> Review.Test.run TwoFer.hasFunctionSignature
                    |> Review.Test.expectErrors
                        [ error
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 7 } }
                        ]
        ]


noWithDefault : Test
noWithDefault =
    describe "solutions that don't use withDefault" <|
        [ test "using a case statement" <|
            \() ->
                """
module TwoFer exposing (twoFer)

twoFer : Maybe String -> String
twoFer name =
    case name of
        Nothing ->
            "One for you, one for me."

        Just you ->
            "One for "
                ++ you
                ++ ", one for me."
"""
                    |> Review.Test.run TwoFer.usesWithDefault
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            (Comment "Doesn't use withDefault" "elm.two-fer.use_withDefault" Essential Dict.empty)
                            "twoFer"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 7 } }
                        ]
        , test "using a function from Maybe.Extra" <|
            \() ->
                """
module TwoFer exposing (twoFer)

import Maybe.Extra

twoFer : Maybe String -> String
twoFer name =
    "One for "
        ++ Maybe.Extra.withDefaultLazy (\\() -> "you") name
        ++ ", one for me."
"""
                    |> Review.Test.run TwoFer.usesWithDefault
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            (Comment "Doesn't use withDefault" "elm.two-fer.use_withDefault" Essential Dict.empty)
                            "twoFer"
                            |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 7 } }
                        ]
        ]
