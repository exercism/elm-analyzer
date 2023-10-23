module Common.SimplifyTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Common.Simplify
import Dict
import Expect
import Json.Decode as Decode
import Review.Rule exposing (Rule)
import Review.Test
import Simplify
import Test exposing (Test, describe, test)
import TestHelper


tests =
    describe "SimplifyTest"
        [ adequateCode, simplify ]


rules : List Rule
rules =
    [ Simplify.rule Simplify.defaults ]


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


simplify : Test
simplify =
    describe "should suggest code simplification" <|
        [ test "Simplify base behavior" <|
            \() ->
                """
module TwoFer exposing (..)

one =
  1 * 1
"""
                    |> Review.Test.run (Simplify.rule Simplify.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary multiplication by 1"
                            , details = [ "Multiplying by 1 does not change the value of the number." ]
                            , under = "* 1"
                            }
                            |> Review.Test.whenFixed "\nmodule TwoFer exposing (..)\n\none =\n  1\n"
                        ]
        , test "decoder behavior" <|
            \() ->
                Decode.decodeString (Common.Simplify.simplifyDecoder (Comment "Simplify" "elm.common.simplify" Actionable Dict.empty)) """
{
"rule": "Simplify",
"message": "Unnecessary multiplication by 1",
"ruleLink": "https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/2.0.9/Simplify",
"details": [
"Multiplying by 1 does not change the value of the number."
],
"region": {
    "start": { "line": 43, "column": 3},
    "end": { "line": 43, "column": 7 }
},
"fix": [
{
    "range": {
        "start": { "line": 43, "column": 3 },
        "end": { "line": 43, "column": 7 }
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
    "string": "Simplify",
    "color": "#FF0000",
    "href": "https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/2.0.9/Simplify"
},
": Unnecessary multiplication by 1\\n\\n42| one =\\n43|  1 * 1\\n      ",
{
    "string": "^^^^",
    "color": "#FF0000"
},
"\\n\\nMultiplying by 1 does not change the value of the number."
],
"suppressed": false,
"originallySuppressed": false
}
"""
                    |> Expect.equal
                        (Ok
                            (Comment "Simplify"
                                "elm.common.simplify"
                                Actionable
                                (Dict.singleton "message" "Simplify: Unnecessary multiplication by 1\n\n42| one =\n43|  1 * 1\n      ^^^^\n\nMultiplying by 1 does not change the value of the number.")
                            )
                        )
        ]
