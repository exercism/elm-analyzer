module Common.NoUnused exposing
    ( customTypeConstructorArgsDecoder
    , customTypeConstructorsDecoder
    , parametersDecoder
    , patternsDecoder
    , ruleConfig
    , variablesDecoder
    )

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
        [ ImportedRule (NoUnused.CustomTypeConstructors.rule []) customTypeConstructorsDecoder
        , ImportedRule NoUnused.CustomTypeConstructorArgs.rule customTypeConstructorArgsDecoder
        , ImportedRule NoUnused.Variables.rule variablesDecoder
        , ImportedRule NoUnused.Parameters.rule parametersDecoder
        , ImportedRule NoUnused.Patterns.rule patternsDecoder
        ]
    }


customTypeConstructorsDecoder : Decoder Comment
customTypeConstructorsDecoder =
    makeDecoder "NoUnused.CustomTypeConstructors" "elm.common.no_unused.custom_type_constructors"


variablesDecoder : Decoder Comment
variablesDecoder =
    makeDecoder "NoUnused.Variables" "elm.common.no_unused.variables"


parametersDecoder : Decoder Comment
parametersDecoder =
    makeDecoder "NoUnused.Parameters" "elm.common.no_unused.parameters"


patternsDecoder : Decoder Comment
patternsDecoder =
    makeDecoder "NoUnused.Patterns" "elm.common.no_unused.patterns"


customTypeConstructorArgsDecoder : Decoder Comment
customTypeConstructorArgsDecoder =
    makeDecoder "NoUnused.CustomTypeConstructorArgs" "elm.common.no_unused.custom_type_constructor_args"


makeDecoder : String -> String -> Decoder Comment
makeDecoder rule path =
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
            if errorRule == rule then
                Comment rule
                    path
                    Actionable
                    (Dict.singleton "definition" (extractCodeLines formatted))
                    |> Decode.succeed

            else
                Decode.fail ("not " ++ rule)
    in
    Decode.map2 Tuple.pair
        (Decode.field "rule" Decode.string)
        (Decode.field "formatted" formattedStringsDecoder)
        |> Decode.andThen toComment
