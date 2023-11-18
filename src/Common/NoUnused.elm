module Common.NoUnused exposing (makeDecoder, ruleConfig)

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
    { restrictToFiles = Nothing
    , rules =
        -- do not include Modules.rule since exercise modules are always unused
        -- do not include Exports.rule since exported functions are always unused
        -- do not include Dependencies.rule since elm.json is standardized
        [ ImportedRule (NoUnused.CustomTypeConstructors.rule [])
            (makeDecoder "NoUnused.CustomTypeConstructors")
            (Comment "elm.common.no_unused.custom_type_constructors" Actionable Dict.empty)
        , ImportedRule NoUnused.CustomTypeConstructorArgs.rule
            (makeDecoder "NoUnused.CustomTypeConstructorArgs")
            (Comment "elm.common.no_unused.custom_type_constructor_args" Actionable Dict.empty)
        , ImportedRule NoUnused.Variables.rule
            (makeDecoder "NoUnused.Variables")
            (Comment "elm.common.no_unused.variables" Actionable Dict.empty)
        , ImportedRule NoUnused.Parameters.rule
            (makeDecoder "NoUnused.Parameters")
            (Comment "elm.common.no_unused.parameters" Actionable Dict.empty)
        , ImportedRule NoUnused.Patterns.rule
            (makeDecoder "NoUnused.Patterns")
            (Comment "elm.common.no_unused.patterns" Actionable Dict.empty)
        ]
    }


makeDecoder : String -> Comment -> Decoder Comment
makeDecoder name comment =
    let
        formattedStringsDecoder : Decoder String
        formattedStringsDecoder =
            Decode.oneOf [ Decode.string, Decode.field "string" Decode.string ]
                |> Decode.list
                |> Decode.map String.concat

        extractCodeLines : String -> String
        extractCodeLines =
            String.split "\n"
                >> List.filter (\line -> startsWithLineNumber line || String.endsWith "^" line)
                >> String.join "\n"

        startsWithLineNumber : String -> Bool
        startsWithLineNumber line =
            case line |> String.trimLeft |> String.split "|" of
                number :: _ ->
                    String.toInt number /= Nothing

                _ ->
                    False

        toComment : ( String, String ) -> Decoder Comment
        toComment ( errorRule, formatted ) =
            if errorRule == name then
                Decode.succeed { comment | params = Dict.singleton "definition" (extractCodeLines formatted) }

            else
                Decode.fail ("not " ++ name)
    in
    Decode.map2 Tuple.pair
        (Decode.field "rule" Decode.string)
        (Decode.field "formatted" formattedStringsDecoder)
        |> Decode.andThen toComment
