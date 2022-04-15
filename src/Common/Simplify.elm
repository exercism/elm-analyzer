module Common.Simplify exposing (ruleConfig, simplifyDecoder)

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
    , rules = [ ImportedRule (Simplify.rule Simplify.defaults) simplifyDecoder ]
    }


simplifyDecoder : Decoder Comment
simplifyDecoder =
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
                    Actionable
                    (Dict.singleton "message"
                        (formatted
                            |> String.concat
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
