module Common.NoUnused exposing (ruleConfig)

import Comment exposing (Comment, CommentType(..))
import Dict
import Json.Decode as Decode exposing (Decoder)
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)



{-

   Rule source code and tests:
       https://github.com/jfmengels/elm-review-unused

-}


ruleConfig : RuleConfig
ruleConfig =
    { slug = Nothing
    , restrictToFiles = Nothing
    , rules =
        -- do not include Modules.rule since exercise modules are always unused
        -- do not include Exports.rule since exported functions are always unused
        -- do not include Dependencies.rule since elm.json is standardized
        -- TODO: Add unit tests for decoders
        [ ImportedRule (NoUnused.CustomTypeConstructors.rule [])
            (makeDecoder "NoUnused.CustomTypeConstructors"
                "It might be handled everywhere it might appear, but there is no location where this value actually gets created."
            )

        -- TODO: message doesn't contain the argument name or location
        , ImportedRule NoUnused.CustomTypeConstructorArgs.rule
            (makeDecoder "NoUnused.CustomTypeConstructorArgs"
                "You should either use it somewhere, or remove it."
            )
        , ImportedRule NoUnused.Variables.rule
            (makeDecoder "NoUnused.Variables"
                "You should either use it somewhere, or remove it."
            )
        , ImportedRule NoUnused.Parameters.rule
            (makeDecoder "NoUnused.Parameters"
                "You should either use it somewhere, or remove it."
            )
        , ImportedRule NoUnused.Patterns.rule
            (makeDecoder "NoUnused.Patterns"
                "You should either use this value somewhere or replace it with '_'."
            )
        ]
    }


makeDecoder : String -> String -> Decoder Comment
makeDecoder rule details =
    let
        toComment : ( String, String ) -> Decoder Comment
        toComment ( errorRule, message ) =
            if errorRule == rule then
                Comment rule
                    "elm.common.no_unused"
                    Informative
                    (Dict.singleton "message" (message ++ "\n\n" ++ details))
                    |> Decode.succeed

            else
                Decode.fail ("not " ++ rule)
    in
    Decode.map2 Tuple.pair
        (Decode.field "rule" Decode.string)
        (Decode.field "message" Decode.string)
        |> Decode.andThen toComment
