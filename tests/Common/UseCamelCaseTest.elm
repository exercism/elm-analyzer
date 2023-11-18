module Common.UseCamelCaseTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Common.UseCamelCase
import Dict
import Expect
import Json.Decode as Decode
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestHelper
import UseCamelCase


tests : Test
tests =
    describe "UseCamelCaseTest"
        [ adequateCode, notCamelCase ]


rules : List Rule
rules =
    [ UseCamelCase.rule UseCamelCase.default ]


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


notCamelCase : Test
notCamelCase =
    describe "should suggest a correct name" <|
        [ test "wrong argument" <|
            \() ->
                """
module TwoFer exposing (..)

twoFer : Maybe String -> String
twoFer my_name =
    "One for "
        ++ Maybe.withDefault "you" my_name
        ++ ", one for me."
"""
                    |> Review.Test.run (UseCamelCase.rule UseCamelCase.default)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Wrong case style for `my_name` argument."
                            , details =
                                [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
                                , "All arguments must be named using the camelCase style.  For this argument that would be `myName`."
                                ]
                            , under = "my_name"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 15 } }
                        ]
        , test "decoder behavior for wrong argument" <|
            \() ->
                Decode.decodeString (Common.UseCamelCase.useCameCaseDecoder (Comment "elm.common.useCamelCase" Actionable Dict.empty)) """
{
  "rule": "UseCamelCase",
  "message": "Wrong case style for `my_name` argument.",
  "ruleLink": "https://package.elm-lang.org/packages/sparksp/elm-review-camelcase/1.1.0/UseCamelCase",
  "details": [
    "It's important to maintain consistent code style to reduce the effort needed to read and understand your code.",
    "All arguments must be named using the camelCase style.  For this argument that would be `myName`."
  ],
  "region": {
    "start": {
      "line": 71,
      "column": 8
    },
    "end": {
      "line": 71,
      "column": 15
    }
  },
  "formatted": [
    {
      "string": "UseCamelCase",
      "color": "#FF0000",
      "href": "https://package.elm-lang.org/packages/sparksp/elm-review-camelcase/1.1.0/UseCamelCase"
    },
    ": Wrong case style for `my_name` argument.\\n\\n70| twoFer : Maybe String -> String\\n71| twoFer my_name =\\n           ",
    {
      "string": "^^^^^^^",
      "color": "#FF0000"
    },
    "\\n72|     \\"One for \\"\\n\\nIt's important to maintain consistent code style to reduce the effort needed to read and understand your code.\\n\\nAll arguments must be named using the camelCase style.  For this argument that would be `myName`."
  ],
  "suppressed": false,
  "originallySuppressed": false
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "elm.common.useCamelCase"
                                Actionable
                                (Dict.fromList [ ( "wrong", "my_name" ), ( "correct", "myName" ) ])
                            )
                        )
        , test "wrong function name" <|
            \() ->
                """
module TwoFer exposing (..)

two_fer : Maybe String -> String
two_fer name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."
"""
                    |> Review.Test.run (UseCamelCase.rule UseCamelCase.default)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Wrong case style for `two_fer` function."
                            , details =
                                [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
                                , "All functions must be named using the camelCase style.  For this function that would be `twoFer`."
                                ]
                            , under = "two_fer"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
                        ]
        , test "decoder behavior for wrong function name" <|
            \() ->
                Decode.decodeString (Common.UseCamelCase.useCameCaseDecoder (Comment "elm.common.useCamelCase" Actionable Dict.empty)) """
{
  "rule": "UseCamelCase",
  "message": "Wrong case style for `two_fer` function.",
  "details": [
    "It's important to maintain consistent code style to reduce the effort needed to read and understand your code.",
    "All functions must be named using the camelCase style.  For this function that would be `twoFer`."
  ]
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "elm.common.useCamelCase"
                                Actionable
                                (Dict.fromList [ ( "wrong", "two_fer" ), ( "correct", "twoFer" ) ])
                            )
                        )
        , test "wrong type" <|
            \() ->
                """
module TwoFer exposing (..)

type My_Type = Snake
"""
                    |> Review.Test.run (UseCamelCase.rule UseCamelCase.default)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Wrong case style for `My_Type` type."
                            , details =
                                [ "It's important to maintain consistent code style to reduce the effort needed to read and understand your code."
                                , "All types must be named using the PascalCase style.  For this type that would be `MyType`."
                                ]
                            , under = "My_Type"
                            }
                        ]
        , test "decoder behavior for wrong type" <|
            \() ->
                Decode.decodeString (Common.UseCamelCase.useCameCaseDecoder (Comment "elm.common.useCamelCase" Actionable Dict.empty)) """
{
  "rule": "UseCamelCase",
  "message": "Wrong case style for `My_Type` type.",
  "ruleLink": "https://package.elm-lang.org/packages/sparksp/elm-review-camelcase/1.1.0/UseCamelCase",
  "details": [
    "It's important to maintain consistent code style to reduce the effort needed to read and understand your code.",
    "All types must be named using the PascalCase style.  For this type that would be `MyType`."
  ]
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "elm.common.useCamelCase"
                                Actionable
                                (Dict.fromList [ ( "wrong", "My_Type" ), ( "correct", "MyType" ) ])
                            )
                        )
        ]
