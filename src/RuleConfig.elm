module RuleConfig exposing (AnalyzerRule(..), RuleConfig, analyzerRuleToRule, getComments, getDecoders, makeConfig)

import Comment exposing (Comment)
import Json.Decode exposing (Decoder)
import Review.Rule as Rule exposing (Rule)


type alias RuleConfig =
    { restrictToFiles : Maybe (List String)
    , rules : List AnalyzerRule
    }


type AnalyzerRule
    = CustomRule (Comment -> Rule) Comment
    | ImportedRule Rule (Comment -> Decoder Comment) Comment
    | TagRule Rule


analyzerRuleToRule : AnalyzerRule -> Rule
analyzerRuleToRule analyzerRule =
    case analyzerRule of
        CustomRule toRule comment ->
            toRule comment

        ImportedRule rule _ _ ->
            rule

        TagRule rule ->
            rule


analyzerRuleToDecoder : AnalyzerRule -> Maybe (Decoder Comment)
analyzerRuleToDecoder analyzerRule =
    case analyzerRule of
        CustomRule _ _ ->
            Nothing

        ImportedRule _ toDecoder comment ->
            Just (toDecoder comment)

        TagRule _ ->
            Nothing


analyzerRuleToComment : AnalyzerRule -> Maybe Comment
analyzerRuleToComment analyzerRule =
    case analyzerRule of
        CustomRule _ comment ->
            Just comment

        ImportedRule _ _ comment ->
            Just comment

        TagRule _ ->
            Nothing


getRules : RuleConfig -> List Rule
getRules { rules, restrictToFiles } =
    case restrictToFiles of
        Nothing ->
            List.map analyzerRuleToRule rules

        Just files ->
            List.map (analyzerRuleToRule >> Rule.filterErrorsForFiles (\file -> List.member file files)) rules


getDecoders : List RuleConfig -> List (Decoder Comment)
getDecoders =
    List.concatMap (.rules >> List.filterMap analyzerRuleToDecoder)


getComments : List RuleConfig -> List Comment
getComments =
    List.concatMap (.rules >> List.filterMap analyzerRuleToComment)


makeConfig : List RuleConfig -> List Rule
makeConfig =
    List.concatMap getRules
