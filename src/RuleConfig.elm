module RuleConfig exposing (AnalyzerRule(..), RuleConfig, analyzerRuleToRule, getDecoders, makeConfig)

import Comment exposing (Comment)
import Json.Decode exposing (Decoder)
import Review.Rule as Rule exposing (Rule)


type alias RuleConfig =
    { slug : Maybe String
    , restrictToFiles : Maybe (List String)
    , rules : List AnalyzerRule
    }


type AnalyzerRule
    = CustomRule (Comment -> Rule) Comment
    | ImportedRule Rule (Comment -> Decoder Comment) Comment


analyzerRuleToRule : AnalyzerRule -> Rule
analyzerRuleToRule analyzerRule =
    case analyzerRule of
        CustomRule toRule comment ->
            toRule comment

        ImportedRule rule _ _ ->
            rule


analyzerRuleToDecoder : AnalyzerRule -> Maybe (Decoder Comment)
analyzerRuleToDecoder analyzerRule =
    case analyzerRule of
        CustomRule _ _ ->
            Nothing

        ImportedRule _ toDecoder comment ->
            Just (toDecoder comment)


getRules : RuleConfig -> List Rule
getRules { rules, restrictToFiles } =
    case restrictToFiles of
        Nothing ->
            List.map analyzerRuleToRule rules

        Just files ->
            List.map (analyzerRuleToRule >> Rule.filterErrorsForFiles (\file -> List.member file files)) rules


getDecoders : RuleConfig -> List (Decoder Comment)
getDecoders { rules } =
    List.filterMap analyzerRuleToDecoder rules


makeConfig : List RuleConfig -> List Rule
makeConfig =
    List.concatMap getRules
