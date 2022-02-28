module Common.Simplify exposing (ruleConfig)

import Comment exposing (Comment, CommentType(..))
import Dict
import Json.Decode as Decode exposing (Decoder)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)
import Simplify



{-
   Rule source code and tests:
      https://github.com/jfmengels/elm-review-simplify

-}


ruleConfig : RuleConfig
ruleConfig =
    { slug = Nothing
    , restrictToFiles = Nothing
    , rules = [ ImportedRule (Simplify.rule Simplify.defaults) decodeSimplify ]
    }



-- TODO: Add unit tests for decoder


decodeSimplify : Decoder Comment
decodeSimplify =
    let
        decodeFormatted : Decoder (List String)
        decodeFormatted =
            Decode.oneOf
                [ Decode.field "string" Decode.string
                , Decode.string
                ]
                |> Decode.list

        toComment : ( String, List String ) -> Decoder Comment
        toComment ( errorRule, formatted ) =
            if errorRule == "Simplify" then
                Comment "Simplify"
                    "elm.common.simplify"
                    Informative
                    (Dict.singleton "message"
                        (formatted
                            |> String.join ""
                            |> String.replace "(fix) " ""
                        )
                    )
                    |> Decode.succeed

            else
                Decode.fail "not Simplify"
    in
    Decode.map2 Tuple.pair
        (Decode.field "rule" Decode.string)
        (Decode.field "formatted" decodeFormatted)
        |> Decode.andThen toComment
