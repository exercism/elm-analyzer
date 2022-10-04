module Common.NoUnusedTest exposing (..)

import Comment exposing (Comment, CommentType(..))
import Common.NoUnused as NoUnused
import Dict
import Expect
import Json.Decode as Decode
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestHelper


rules : List Rule
rules =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]


adequateCode : Test
adequateCode =
    test "should not report anything for adequate code" <|
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


customTypeConstructors : Test
customTypeConstructors =
    describe "should detect unused type constructors" <|
        [ test "NoUnused.CustomTypeConstructors base behavior" <|
            \() ->
                """
module TwoFer exposing (..)

type TwoFer = TwoFerUnused
"""
                    |> Review.Test.run (NoUnused.CustomTypeConstructors.rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `TwoFerUnused` is not used."
                            , details = [ "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created." ]
                            , under = "TwoFerUnused"
                            }
                        ]
        , test "decoder behavior" <|
            \() ->
                Decode.decodeString NoUnused.customTypeConstructorsDecoder """
{
"rule": "NoUnused.CustomTypeConstructors",
"message": "Type constructor `TwoFerUnused` is not used.",
"ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-CustomTypeConstructors",
"details": [
"This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."
],
"region": {
    "start": { "line": 9, "column": 15 },
    "end": { "line": 9, "column": 27 }
},
"formatted": [
{
    "string": "NoUnused.CustomTypeConstructors",
    "color": "#FF0000",
    "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-CustomTypeConstructors"
},
": Type constructor `TwoFerUnused` is not used.\\n\\n 8| -- unused type constructor TwoFerUnused\\n 9| type TwoFer = TwoFerUnused\\n                  ",
{ "string": "^^^^^^^^^^^^", "color": "#FF0000" },
"\\n\\nThis type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."
],
"suppressed": false,
"originallySuppressed": false
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "NoUnused.CustomTypeConstructors"
                                "elm.common.no_unused.custom_type_constructors"
                                Actionable
                                (Dict.singleton "definition" " 8| -- unused type constructor TwoFerUnused\n 9| type TwoFer = TwoFerUnused\n                  ^^^^^^^^^^^^")
                            )
                        )
        ]


customTypeConstructorArgs : Test
customTypeConstructorArgs =
    describe "should detect unused type constructor arguments" <|
        [ test "NoUnused.CustomTypeConstructorArgs base behavior" <|
            \() ->
                """
module TwoFer exposing (..)

type TwoFer = TwoFerUnused String
"""
                    |> Review.Test.run NoUnused.CustomTypeConstructorArgs.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Argument is never extracted and therefore never used."
                            , details = [ "This argument is never used. You should either use it somewhere, or remove it at the location I pointed at." ]
                            , under = "String"
                            }
                        ]
        , test "decoder behavior" <|
            \() ->
                Decode.decodeString NoUnused.customTypeConstructorArgsDecoder """
{
"rule": "NoUnused.CustomTypeConstructorArgs",
"message": "Argument is never extracted and therefore never used.",
"ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-CustomTypeConstructorArgs",
"details": [
"This argument is never used. You should either use it somewhere, or remove it at the location I pointed at."
],
"region": {
    "start": { "line": 12, "column": 31 },
    "end": { "line": 12, "column": 37 }
},
"formatted": [
{
    "string": "NoUnused.CustomTypeConstructorArgs",
    "color": "#FF0000",
    "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-CustomTypeConstructorArgs"
},
": Argument is never extracted and therefore never used.\\n\\n11| -- unused type constructor argument String\\n12| type Unused = TwoFerUnusedArg String\\n                                  ",
{ "string": "^^^^^^", "color": "#FF0000" },
"\\n\\nThis argument is never used. You should either use it somewhere, or remove it at the location I pointed at."
],
"suppressed": false,
"originallySuppressed": false
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "NoUnused.CustomTypeConstructorArgs"
                                "elm.common.no_unused.custom_type_constructor_args"
                                Actionable
                                (Dict.singleton "definition" "11| -- unused type constructor argument String\n12| type Unused = TwoFerUnusedArg String\n                                  ^^^^^^")
                            )
                        )
        ]


variables : Test
variables =
    describe "should detect unused variables" <|
        [ test "NoUnused.Variables base behavior" <|
            \() ->
                """
module TwoFer exposing (..)

import Parser.Advanced
"""
                    |> Review.Test.run NoUnused.Variables.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Imported module `Parser.Advanced` is not used"
                            , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                            , under = "Parser.Advanced"
                            }
                            |> Review.Test.whenFixed "\nmodule TwoFer exposing (..)\n\n"
                        ]
        , test "decoder behavior" <|
            \() ->
                Decode.decodeString NoUnused.variablesDecoder """
{
"rule": "NoUnused.Variables",
"message": "Imported module `Parser.Advanced` is not used",
"ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-Variables",
"details": [
"You should either use this value somewhere, or remove it at the location I pointed at."
],
"region": {
    "start": { "line": 6, "column": 8 },
    "end": { "line": 6, "column": 23 }
},
"fix": [
    {
        "range": {
            "start": { "line": 6, "column": 1 },
            "end": { "line": 7, "column": 1 }
        },
        "string": ""
    }
],
"formatted": [
    { "string": "(fix) ", "color": "#33BBC8" },
    {
        "string": "NoUnused.Variables",
        "color": "#FF0000",
        "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-Variables"
    },
    ": Imported module `Parser.Advanced` is not used\\n\\n5| -- unused imported module\\n6| import Parser.Advanced\\n          ",
    { "string": "^^^^^^^^^^^^^^^", "color": "#FF0000" },
    "\\n\\nYou should either use this value somewhere, or remove it at the location I pointed at."
],
"suppressed": false,
"originallySuppressed": false
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "NoUnused.Variables"
                                "elm.common.no_unused.variables"
                                Actionable
                                (Dict.singleton "definition" "5| -- unused imported module\n6| import Parser.Advanced\n          ^^^^^^^^^^^^^^^")
                            )
                        )
        ]


parameters : Test
parameters =
    describe "should detect unused parameters" <|
        [ test "NoUnused.Parameters base behavior" <|
            \() ->
                """
module TwoFer exposing (..)

unusedParameter unused = 
  Nothing
"""
                    |> Review.Test.run NoUnused.Parameters.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Parameter `unused` is not used"
                            , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
                            , under = "unused"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 4, column = 17 }, end = { row = 4, column = 23 } }
                        ]
        , test "decoder behavior" <|
            \() ->
                Decode.decodeString NoUnused.parametersDecoder """
{
"rule": "NoUnused.Parameters",
"message": "Parameter `unused` is not used",
"ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-Parameters",
"details": [
"You should either use this parameter somewhere, or remove it at the location I pointed at."
],
"region": {
    "start": { "line": 33, "column": 17 },
    "end": { "line": 33, "column": 23 }
},
"formatted": [
{
    "string": "NoUnused.Parameters",
    "color": "#FF0000",
    "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-Parameters"
},
": Parameter `unused` is not used\\n\\n32| -- unused is unused\\n33| unusedParameter unused =\\n                    ",
{ "string": "^^^^^^", "color": "#FF0000" },
"\\n34|   Nothing\\n\\nYou should either use this parameter somewhere, or remove it at the location I pointed at."
],
"suppressed": false,
"originallySuppressed": false
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "NoUnused.Parameters"
                                "elm.common.no_unused.parameters"
                                Actionable
                                (Dict.singleton "definition" "32| -- unused is unused\n33| unusedParameter unused =\n                    ^^^^^^\n34|   Nothing")
                            )
                        )
        ]


patterns : Test
patterns =
    describe "should detect unused patterns" <|
        [ test "NoUnused.Patterns base behavior" <|
            \() ->
                """
module TwoFer exposing (..)

f x =
  case x of
    Just something -> 1
    Nothing -> 0
"""
                    |> Review.Test.run NoUnused.Patterns.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Value `something` is not used"
                            , details = [ "You should either use this value somewhere or replace it with '_'." ]
                            , under = "something"
                            }
                            |> Review.Test.whenFixed "\nmodule TwoFer exposing (..)\n\nf x =\n  case x of\n    Just _ -> 1\n    Nothing -> 0\n"
                        ]
        , test "decoder behavior" <|
            \() ->
                Decode.decodeString NoUnused.patternsDecoder """
{
"rule": "NoUnused.Patterns",
"message": "Value `something` is not used",
"ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-Patterns",
"details": [
"You should either use this value somewhere or replace it with '_'."
],
"region": {
    "start": { "line": 35, "column": 10 },
    "end": { "line": 35, "column": 19 }
},
"fix": [
    {
        "range": {
            "start": { "line": 35, "column": 10 },
            "end": { "line": 35, "column": 19 }
        },
        "string": "_"
    }
    ],
"formatted": [
{ "string": "(fix) ", "color": "#33BBC8" },
{
    "string": "NoUnused.Patterns",
    "color": "#FF0000",
    "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.20/NoUnused-Patterns"
},
": Value `something` is not used\\n\\n34|  case x of\\n35|    Just something -> 1\\n             ",
{ "string": "^^^^^^^^^", "color": "#FF0000" },
"\\n36|    Nothing -> 0\\n\\nYou should either use this value somewhere or replace it with '_'."
],
"suppressed": false,
"originallySuppressed": false
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "NoUnused.Patterns"
                                "elm.common.no_unused.patterns"
                                Actionable
                                (Dict.singleton "definition" "34|  case x of\n35|    Just something -> 1\n             ^^^^^^^^^\n36|    Nothing -> 0")
                            )
                        )
        ]
