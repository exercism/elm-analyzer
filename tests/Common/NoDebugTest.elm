module Common.NoDebugTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Common.NoDebug
import Dict
import Expect
import Json.Decode as Decode
import NoDebug.Log
import NoDebug.TodoOrToString
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "NoDebugTest"
        [ noDebug
        , withDebugLog
        , withDebugTodo
        , withDebugToString
        ]


rules : List Rule
rules =
    Common.NoDebug.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


noDebug : Test
noDebug =
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


withDebugLog : Test
withDebugLog =
    describe "detects the presence of a Debug.log"
        [ test "imported rule works" <|
            \() ->
                """
module TwoFer exposing (twoFer)

twoFer : Maybe String -> String
twoFer name =
        "One for "
            ++ Maybe.withDefault "you" (Debug.log "name" name)
            ++ ", one for me."
"""
                    |> Review.Test.run NoDebug.Log.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Remove the use of `Debug.log` before shipping to production"
                            , details = [ "`Debug.log` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                            , under = "Debug.log"
                            }
                            |> Review.Test.whenFixed "\nmodule TwoFer exposing (twoFer)\n\ntwoFer : Maybe String -> String\ntwoFer name =\n        \"One for \"\n            ++ Maybe.withDefault \"you\" (name)\n            ++ \", one for me.\"\n"
                        ]
        , test "decoder behavior" <|
            \() ->
                let
                    comment =
                        Comment "NoDebug.Log" "elm.common.no_debug" Actionable Dict.empty
                in
                Decode.decodeString (Common.NoDebug.noDebugDecoder comment) """
{
  "rule": "NoDebug.Log",
  "message": "Remove the use of `Debug.log` before shipping to production",
  "ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-debug/1.0.8/NoDebug-Log",
  "details": [
    "`Debug.log` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
  ],
  "region": {
    "start": {
      "line": 53,
      "column": 37
    },
    "end": {
      "line": 53,
      "column": 46
    }
  },
  "fix": [
    {
      "range": {
        "start": {
          "line": 53,
          "column": 37
        },
        "end": {
          "line": 53,
          "column": 54
        }
      },
      "string": ""
    }
  ],
  "formatted": [
    {
      "string": "(fix) ",
      "color": "#33BBC8"
    },
    {
      "string": "NoDebug.Log",
      "color": "#FF0000",
      "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-debug/1.0.8/NoDebug-Log"
    },
    ": Remove the use of `Debug.log` before shipping to production\\n\\n52|     \\"One for \\"\\n53|         ++ Maybe.withDefault \\"you\\" (Debug.log \\"name\\" name)\\n                                        ",
    {
      "string": "^^^^^^^^^",
      "color": "#FF0000"
    },
    "\\n54|         ++ \\", one for me.\\"\\n\\n`Debug.log` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
  ],
  "suppressed": false,
  "originallySuppressed": false
}
"""
                    |> Expect.equal (Ok comment)
        ]


withDebugTodo : Test
withDebugTodo =
    describe "detects the presence of a Debug.todo"
        [ test "imported rule works" <|
            \() ->
                """
module TwoFer exposing (twoFer)

twoFer : Maybe String -> String
twoFer name =
    if name == Just "Cedd" then
        Debug.todo "think of something funny later"
    else
        "One for "
            ++ Maybe.withDefault "you" name
            ++ ", one for me."
"""
                    |> Review.Test.run NoDebug.TodoOrToString.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Remove the use of `Debug.todo` before shipping to production"
                            , details = [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                            , under = "Debug.todo"
                            }
                        ]
        , test "decoder behavior" <|
            \() ->
                let
                    comment =
                        Comment "NoDebug.TodoOrToString" "elm.common.no_debug" Actionable Dict.empty
                in
                Decode.decodeString (Common.NoDebug.noDebugDecoder comment) """
{
    "rule": "NoDebug.TodoOrToString",
    "message": "Remove the use of `Debug.todo` before shipping to production",
    "ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-debug/1.0.8/NoDebug-TodoOrToString",
    "details": [
    "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
    ],
    "region": {
    "start": {
        "line": 48,
        "column": 12
    },
    "end": {
        "line": 48,
        "column": 22
    }
    },
    "formatted": [
    {
        "string": "NoDebug.TodoOrToString",
        "color": "#FF0000",
        "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-debug/1.0.8/NoDebug-TodoOrToString"
    },
    ": Remove the use of `Debug.todo` before shipping to production\\n47|         |> Debug.todo \\"think of something funny later\\"\\n",
    {
        "string": "^^^^^^^^^^",
        "color": "#FF0000"
    },
    "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
    ],
    "suppressed": false,
    "originallySuppressed": false
}
    """
                    |> Expect.equal (Ok comment)
        ]


withDebugToString : Test
withDebugToString =
    describe "detects the presence of a Debug.toString"
        [ test "imported rule works" <|
            \() ->
                """
module TwoFer exposing (twoFer)

twoFer : Maybe String -> String
twoFer name =
        "One for "
            ++ Debug.toString name
            ++ ", one for me."
"""
                    |> Review.Test.run NoDebug.TodoOrToString.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Remove the use of `Debug.toString` before shipping to production"
                            , details = [ "`Debug.toString` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                            , under = "Debug.toString"
                            }
                        ]
        , test "decoder behavior" <|
            \() ->
                let
                    comment =
                        Comment "NoDebug.TodoOrToString" "elm.common.no_debug" Actionable Dict.empty
                in
                Decode.decodeString (Common.NoDebug.noDebugDecoder comment) """
{
  "rule": "NoDebug.TodoOrToString",
  "message": "Remove the use of `Debug.toString` before shipping to production",
  "ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-debug/1.0.8/NoDebug-TodoOrToString",
  "details": [
    "`Debug.toString` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
  ],
  "region": {
    "start": {
      "line": 53,
      "column": 12
    },
    "end": {
      "line": 53,
      "column": 26
    }
  },
  "formatted": [
    {
      "string": "NoDebug.TodoOrToString",
      "color": "#FF0000",
      "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-debug/1.0.8/NoDebug-TodoOrToString"
    },
    ": Remove the use of `Debug.toString` before shipping to production\\n\\n52|     \\"One for \\"\\n53|         ++ Debug.toString name\\n               ",
    {
      "string": "^^^^^^^^^^^^^^",
      "color": "#FF0000"
    },
    "\\n54|         ++ \\", one for me.\\"\\n\\n`Debug.toString` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
  ],
  "suppressed": false,
  "originallySuppressed": false
}
"""
                    |> Expect.equal (Ok comment)
        ]
