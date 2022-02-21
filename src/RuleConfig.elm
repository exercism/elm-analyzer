module RuleConfig exposing (AnalyzerRule(..), RuleConfig, getDecoders, makeConfig)

import Comment exposing (Comment)
import Json.Decode exposing (Decoder)
import Review.Rule as Rule exposing (Rule)


type alias RuleConfig =
    { slug : Maybe String
    , restrictToFiles : Maybe (List String)
    , rules : List AnalyzerRule
    }


type AnalyzerRule
    = CustomRule Rule
    | ImportedRule Rule (Decoder Comment)


analyzerRuletoRule : AnalyzerRule -> Rule
analyzerRuletoRule analyzerRule =
    case analyzerRule of
        CustomRule rule ->
            rule

        ImportedRule rule _ ->
            rule


analyzerRuletoDecoder : AnalyzerRule -> Maybe (Decoder Comment)
analyzerRuletoDecoder analyzerRule =
    case analyzerRule of
        CustomRule _ ->
            Nothing

        ImportedRule _ decoder ->
            Just decoder


getRules : RuleConfig -> List Rule
getRules { rules, restrictToFiles } =
    case restrictToFiles of
        Nothing ->
            List.map analyzerRuletoRule rules

        Just files ->
            List.map (analyzerRuletoRule >> Rule.filterErrorsForFiles (\file -> List.member file files)) rules


getDecoders : RuleConfig -> List (Decoder Comment)
getDecoders { rules } =
    List.filterMap analyzerRuletoDecoder rules


makeConfig : List RuleConfig -> List Rule
makeConfig ruleConfigs =
    ruleConfigs
        |> List.map getRules
        |> List.concat
