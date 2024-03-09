module Exercise.SieveTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.Sieve as Sieve
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "SieveTest"
        [ exampleSolution
        , usingDivisionOrModulo
        ]


rules : List Rule
rules =
    Sieve.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exampleSolution : Test
exampleSolution =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module Sieve exposing (primes)

import Array exposing (Array)


primes : Int -> List Int
primes limit =
    let
        -- an array, with indices from 0 until limit, each with a Bool value representing primeness
        initialSieve =
            Array.initialize (limit + 1) (
 -> n >= 2)

        initialPrime =
            Just 2
    in
    runSieve initialPrime initialSieve
        |> Array.toIndexedList
        |> List.filterMap
            (\\( index, isPrime ) ->
                if isPrime then
                    Just index

                else
                    Nothing
            )


runSieve : Maybe Int -> Array Bool -> Array Bool
runSieve maybePrime sieve =
    case maybePrime of
        Nothing ->
            sieve

        Just prime ->
            let
                removeMultiples : Int -> Array Bool -> Array Bool
                removeMultiples multiple =
                    if multiple > Array.length sieve then
                        identity

                    else
                        Array.set multiple False
                            >> removeMultiples (multiple + prime)

                sieveWithPrimeMultipleRemoved : Array Bool
                sieveWithPrimeMultipleRemoved =
                    removeMultiples (2 * prime) sieve

                findNextPrime : Int -> Maybe Int
                findNextPrime index =
                    case Array.get index sieveWithPrimeMultipleRemoved of
                        Nothing ->
                            Nothing

                        Just True ->
                            Just index

                        Just False ->
                            findNextPrime (index + 1)
            in
            runSieve (findNextPrime (prime + 1)) sieveWithPrimeMultipleRemoved
"""


usingDivisionOrModulo : Test
usingDivisionOrModulo =
    let
        comment =
            Comment "elm.sieve.do_not_use_division_or_modulo" Essential Dict.empty
    in
    describe "solutions that use division" <|
        [ test "using (/)" <|
            \() ->
                """
module Sieve exposing (primes)

primes : Int -> List Int
primes limit = run [] (List.range 2 limit)

run smallPrimes list =
    case list of
        [] -> List.reverse smallPrimes
        prime :: rest ->
            let newPrimes = prime :: smallPrimes
            in
            List.filterMap
                (\\n -> if List.any (\\p -> ceiling (toFloat n / toFloat p) == floor (toFloat n / toFloat p)) newPrimes then Nothing else Just n)
                rest
                |> run newPrimes
"""
                    |> Review.Test.run (Sieve.doNotUseDivisionOrModulo comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "toFloat n / toFloat p"
                            |> Review.Test.atExactly { start = { row = 14, column = 85 }, end = { row = 14, column = 106 } }
                        ]
        , test "using (//)" <|
            \() ->
                """
module Sieve exposing (primes)

primes : Int -> List Int
primes limit = run [] (List.range 2 limit)

run smallPrimes list =
    case list of
        [] -> List.reverse smallPrimes
        prime :: rest ->
            let newPrimes = prime :: smallPrimes
            in
            List.filterMap
                (\\n -> if List.any (\\p -> (n // p) * p == n) newPrimes then Nothing else Just n)
                rest
                |> run newPrimes
"""
                    |> Review.Test.run (Sieve.doNotUseDivisionOrModulo comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "n // p" ]
        , test "using modBy" <|
            \() ->
                """
module Sieve exposing (primes)

primes : Int -> List Int
primes limit = run [] (List.range 2 limit)

run smallPrimes list =
    case list of
        [] -> List.reverse smallPrimes
        prime :: rest ->
            let newPrimes = prime :: smallPrimes
            in
            List.filterMap
                (\\n -> if List.any (\\p -> modBy p n == 0) newPrimes then Nothing else Just n)
                rest
                |> run newPrimes
"""
                    |> Review.Test.run (Sieve.doNotUseDivisionOrModulo comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "modBy" ]
        , test "using remainderBy" <|
            \() ->
                """
module Sieve exposing (primes)

primes : Int -> List Int
primes limit = run [] (List.range 2 limit)

run smallPrimes list =
    case list of
        [] -> List.reverse smallPrimes
        prime :: rest ->
            let newPrimes = prime :: smallPrimes
            in
            List.filterMap
                (\\n -> if List.any (\\p -> remainderBy p n == 0) newPrimes then Nothing else Just n)
                rest
                |> run newPrimes
"""
                    |> Review.Test.run (Sieve.doNotUseDivisionOrModulo comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "remainderBy" ]
        ]
