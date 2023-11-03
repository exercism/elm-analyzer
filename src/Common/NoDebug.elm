module Common.NoDebug exposing (noDebugDecoder, ruleConfig)

import Comment exposing (Comment, CommentType(..))
import Dict
import Json.Decode as Decode exposing (Decoder)
import NoDebug.Log
import NoDebug.TodoOrToString
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)



{-

   Rule source code and tests:
       https://github.com/jfmengels/elm-review-debug

-}


ruleConfig : RuleConfig
ruleConfig =
    { slug = Nothing
    , restrictToFiles = Nothing
    , rules =
        [ ImportedRule NoDebug.Log.rule
            noDebugDecoder
            (Comment "NoDebug.Log" "elm.common.common.no_debug" Actionable Dict.empty)
        , ImportedRule NoDebug.TodoOrToString.rule
            noDebugDecoder
            (Comment "NoDebug.TodoOrToString" "elm.common.common.no_debug" Actionable Dict.empty)
        ]
    }


noDebugDecoder : Comment -> Decoder Comment
noDebugDecoder comment =
    let
        toComment : String -> Decoder Comment
        toComment errorRule =
            if errorRule == "NoDebug.TodoOrToString" || errorRule == "NoDebug.Log" then
                Decode.succeed comment

            else
                Decode.fail "not NoDebug rule"
    in
    Decode.field "rule" Decode.string
        |> Decode.andThen toComment
