module AnalyzerTest exposing (tests)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict exposing (Dict)
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestHelper


tests =
    describe "AnalyzerTest tests"
        [ calledFromTest, findFunctionsTest, findTest, indirectCallTest ]


allRules : Dict String Rule
allRules =
    let
        called =
            [ ( "called anywhere", Anywhere ), ( "calling function", TopFunction "callingFunction" ) ]

        calling =
            [ ( "Ext._", [ AnyFromExternalModule [ "Ext" ] ] )
            , ( "Ext.ext", [ FromExternalModule [ "Ext" ] "ext" ] )
            , ( "internal", [ FromSameModule "internal" ] )
            , ( "let", [ LetBlock ] )
            , ( "case", [ CaseBlock ] )
            , ( "Ext._ + Ext.ext", [ AnyFromExternalModule [ "Ext" ], FromExternalModule [ "Ext" ] "ext" ] )
            , ( "Ext._ + internal", [ AnyFromExternalModule [ "Ext" ], FromSameModule "internal" ] )
            , ( "Ext.ext + internal", [ FromExternalModule [ "Ext" ] "ext", FromSameModule "internal" ] )
            , ( "Ext._ + Ext.ext + internal", [ AnyFromExternalModule [ "Ext" ], FromExternalModule [ "Ext" ] "ext", FromSameModule "internal" ] )
            ]

        finding =
            [ ( "all", All ), ( "none", None ), ( "some", Some ) ]
    in
    called
        |> List.concatMap
            (\( calledFromName, calledFrom ) ->
                calling
                    |> List.concatMap
                        (\( findFunctionName, findFunctions ) ->
                            finding
                                |> List.map
                                    (\( findName, find ) ->
                                        let
                                            name =
                                                String.join ", " [ calledFromName, findFunctionName, findName ]
                                        in
                                        ( name
                                        , Analyzer.functionCalls
                                            { calledFrom = calledFrom
                                            , findFunctions = findFunctions
                                            , find = find
                                            }
                                            (quickComment name)
                                        )
                                    )
                        )
            )
        |> Dict.fromList


quickComment : String -> Comment
quickComment name =
    Comment name name Essential Dict.empty


getRule : String -> Rule
getRule ruleName =
    Dict.get ruleName allRules
        |> Maybe.withDefault
            (Review.Rule.newModuleRuleSchema "empty rule" ()
                |> Review.Rule.withSimpleExpressionVisitor (always [])
                |> Review.Rule.fromModuleRuleSchema
            )


calledFromTest : Test
calledFromTest =
    let
        noCallingRule =
            "called anywhere, Ext._ + Ext.ext + internal, all"

        callingRule =
            "calling function, Ext._ + Ext.ext + internal, all"
    in
    describe "functionCalls should work with or without calledFrom function"
        [ test "without calledFrom function, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

someRandomFunction param =
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule noCallingRule)
                    |> Review.Test.expectNoErrors
        , test "without calledFrom function, missing function" <|
            \() ->
                """
module A exposing (..)

import Ext

someRandomFunction param =
  param
-- |> Ext.ext call
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule noCallingRule)
                    |> Review.Test.expectGlobalErrors
                        [ quickComment noCallingRule |> TestHelper.createExpectedGlobalError ]
        , test "with calledFrom function, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule callingRule)
                    |> Review.Test.expectNoErrors
        , test "with calledFrom function, missing function" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
-- |> Ext.ext call
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule callingRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment callingRule) "callingFunction" ]
        , test "with calledFrom function, some functions called in let, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  let
    fromLet = internal
  in
  param
  |> (\\x -> let z = Ext.ext in z x)
  |> Ext.other
  |> fromLet
"""
                    |> Review.Test.run (getRule callingRule)
                    |> Review.Test.expectNoErrors
        ]


findTest : Test
findTest =
    let
        allRule =
            "calling function, Ext._ + Ext.ext + internal, all"

        someRule =
            "calling function, Ext._ + Ext.ext + internal, some"

        noneRule =
            "calling function, Ext._ + Ext.ext + internal, none"
    in
    describe "should work with all values of Find"
        [ test "with All, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule allRule)
                    |> Review.Test.expectNoErrors
        , test "with All, missing function" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule allRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment allRule) "callingFunction" ]
        , test "with Some, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
--  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule someRule)
                    |> Review.Test.expectNoErrors
        , test "with Some, missing functions" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
--  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule someRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment someRule) "callingFunction" ]
        , test "with None, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
--  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule noneRule)
                    |> Review.Test.expectNoErrors
        , test "with None, found functions" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule noneRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment noneRule) "Ext.other" ]
        ]


findFunctionsTest : Test
findFunctionsTest =
    let
        extRule =
            "calling function, Ext._, all"

        extExtRule =
            "calling function, Ext.ext, all"

        internalRule =
            "calling function, internal, all"

        letRule =
            "calling function, let, all"

        caseRule =
            "calling function, case, all"

        extExtExtRule =
            "calling function, Ext._ + Ext.ext, all"

        extInternalRule =
            "calling function, Ext._ + internal, all"

        extExtInternalRule =
            "calling function, Ext.ext + internal, all"

        allThreeRule =
            "calling function, Ext._ + Ext.ext + internal, all"
    in
    describe "should work for various values of called functions"
        [ test "any function from Ext, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extRule)
                    |> Review.Test.expectNoErrors
        , test "any function from Ext, missing function" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
--  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment extRule) "callingFunction" ]
        , test "calling Ext.ext, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
--  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule extExtRule)
                    |> Review.Test.expectNoErrors
        , test "calling Ext.ext, missing function" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extExtRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment extExtRule) "callingFunction" ]
        , test "calling internal function, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule internalRule)
                    |> Review.Test.expectNoErrors
        , test "calling internal function, missing function" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule internalRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment internalRule) "callingFunction" ]
        , test "calling let expression, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  let
    internal = identity
  in
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule letRule)
                    |> Review.Test.expectNoErrors
        , test "calling let expression, missing expression" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
--  let
--    internal = identity
--  in
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule letRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment letRule) "callingFunction" ]
        , test "calling case expression, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  case param of
    1 -> Ext.ext
    2 -> Ext.other
    3 -> internal
"""
                    |> Review.Test.run (getRule caseRule)
                    |> Review.Test.expectNoErrors
        , test "calling case expression, missing expression" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
--  case param of
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule caseRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment caseRule) "callingFunction" ]
        , test "any function from Ext and Ext, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule extExtExtRule)
                    |> Review.Test.expectNoErrors
        , test "any function from Ext and Ext.ext, only Ext.ext, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
--  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule extExtExtRule)
                    |> Review.Test.expectNoErrors
        , test "any function from Ext and Ext.ext, missing Ext.ex" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extExtExtRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment extExtExtRule) "callingFunction" ]
        , test "any function from Ext and internal, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extInternalRule)
                    |> Review.Test.expectNoErrors
        , test "any function from Ext and internal, missing Ext" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
--  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extInternalRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment extInternalRule) "callingFunction" ]
        , test "any function from Ext and internal, missing internal" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule extInternalRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment extInternalRule) "callingFunction" ]
        , test "calling Ext.ext + internal, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extExtInternalRule)
                    |> Review.Test.expectNoErrors
        , test "calling Ext.ext + internal, missing Ext.ext" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule extExtInternalRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment extExtInternalRule) "callingFunction" ]
        , test "calling Ext.ext + internal, missing internal" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule extExtInternalRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment extExtInternalRule) "callingFunction" ]
        , test "calling all 3 functions, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectNoErrors
        , test "calling all 3 functions, only Ext.ext and internal, no error" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
--  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectNoErrors
        , test "calling all 3 functions, missing internal" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> Ext.other
--  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment allThreeRule) "callingFunction" ]
        , test "calling all 3 functions, missing Ext.ext" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
--  |> Ext.ext
  |> Ext.other
  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment allThreeRule) "callingFunction" ]
        , test "should detect imported functions" <|
            \() ->
                """
module A exposing (..)

import Ext exposing (other, ext)

callingFunction param =
  param
  |> ext
  |> other
  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectNoErrors
        , test "should detect aliased module" <|
            \() ->
                """
module A exposing (..)

import Ext as NotExt

callingFunction param =
  param
  |> NotExt.ext
  |> NotExt.other
  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectNoErrors
        , test "should detect aliased module with conflicting name" <|
            \() ->
                """
module A exposing (..)

import Ext as List

callingFunction param =
  param
  |> List.ext
  |> List.other
  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectNoErrors
        , test "should detect imported functions from aliased module" <|
            \() ->
                """
module A exposing (..)

import Ext as NotExt exposing (ext, other)

callingFunction param =
  param
  |> ext
  |> other
  |> internal
"""
                    |> Review.Test.run (getRule allThreeRule)
                    |> Review.Test.expectNoErrors
        , test "should detect composite module" <|
            \() ->
                """
module A exposing (..)

import Ext.Sub

callingFunction param =
  param
  |> Ext.Sub.ext
  |> internal
"""
                    |> Review.Test.run
                        (Analyzer.functionCalls
                            { calledFrom = Anywhere
                            , findFunctions = [ AnyFromExternalModule [ "Ext", "Sub" ] ]
                            , find = All
                            }
                            (quickComment "Ext.Sub")
                        )
                    |> Review.Test.expectNoErrors
        , test "should detect imported Basic functions with module name" <|
            \() ->
                """
module A exposing (..)

callingFunction param =
  param
  |> toFloat
  |> round
  |> (\\x -> x == 3)
"""
                    |> Review.Test.run
                        (Analyzer.functionCalls
                            { calledFrom = Anywhere
                            , findFunctions =
                                [ FromExternalModule [ "Basics" ] "toFloat"
                                , FromExternalModule [ "Basics" ] "round"
                                ]
                            , find = All
                            }
                            (quickComment "basics")
                        )
                    |> Review.Test.expectNoErrors
        , test "shouldn't confuse imported Basic functions with module functions" <|
            \() ->
                """
module A exposing (..)

callingFunction param =
  param
  |> toFloat
  |> round
  |> (\\x -> x == 3)
"""
                    |> Review.Test.run
                        (Analyzer.functionCalls
                            { calledFrom = Anywhere
                            , findFunctions =
                                [ FromSameModule "toFloat"
                                , FromSameModule "round"
                                ]
                            , find = Some
                            }
                            (quickComment "basics")
                        )
                    |> Review.Test.expectGlobalErrors
                        [ TestHelper.createExpectedGlobalError (quickComment "basics") ]
        ]


indirectCallTest : Test
indirectCallTest =
    let
        rule =
            "calling function, Ext.ext + internal, all"
    in
    describe "functions should be found when used through helper functions"
        [ test "call in a different function" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param

helperOne param =
  param
  |> Ext.ext
  |> internal
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder (quickComment rule) "callingFunction" ]
        , test "direct call" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> Ext.ext
  |> internal
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectNoErrors
        , test "indirect call" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> helperOne

helperOne param =
  param
  |> Ext.ext
  |> internal
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectNoErrors
        , test "some direct, some indirect call" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> helperOne
  |> Ext.ext

helperOne param =
  param
  |> internal
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectNoErrors
        , test "indirect call twice removed" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> helperOne

helperOne param =
  param
  |> helperTwo

helperTwo param =
  param
  |> Ext.ext
  |> internal
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectNoErrors
        , test "indirect calls at different levels" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> helperOne

helperOne param =
  param
  |> helperTwo
  |> Ext.ext

helperTwo param =
  param
  |> internal
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectNoErrors
        , test "recursive calls are OK" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> helperOne
  |> callingFunction

helperOne param =
  param
  |> internal
  |> Ext.ext
  |> helperOne
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectNoErrors
        , test "co-recursive calls are OK" <|
            \() ->
                """
module A exposing (..)

import Ext

callingFunction param =
  param
  |> helperOne
  |> internal

helperOne param =
  param
  |> callingFunction
  |> Ext.ext
"""
                    |> Review.Test.run (getRule rule)
                    |> Review.Test.expectNoErrors
        ]
