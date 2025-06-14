module Exercise.SplitSecondStopwatch exposing (forbidStopwatchExportAll, forbidStopwatchTypeAlias, ruleConfig)

import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import List.Extra
import Review.Rule exposing (Error, Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/SplitSecondStopwatch.elm" ]
    , rules =
        [ CustomRule forbidStopwatchTypeAlias
            (Comment "elm.split-second-stopwatch.no_stopwatch_type_alias" Essential Dict.empty)
        , CustomRule forbidStopwatchExportAll
            (Comment "elm.split-second-stopwatch.no_stopwatch_export_all" Essential Dict.empty)
        ]
    }


forbidStopwatchTypeAlias : Comment -> Rule
forbidStopwatchTypeAlias comment =
    Review.Rule.newModuleRuleSchema comment.path ()
        |> Review.Rule.withSimpleDeclarationVisitor (hasStopwatchTypeAliasVisitor comment)
        |> Review.Rule.fromModuleRuleSchema


hasStopwatchTypeAliasVisitor : Comment -> Node Declaration -> List (Error {})
hasStopwatchTypeAliasVisitor comment node =
    case Node.value node of
        AliasDeclaration { name } ->
            if Node.value name == "Stopwatch" then
                [ Comment.createError comment (Node.range name) ]

            else
                []

        _ ->
            []


forbidStopwatchExportAll : Comment -> Rule
forbidStopwatchExportAll comment =
    Review.Rule.newModuleRuleSchema comment.path ()
        |> Review.Rule.withSimpleModuleDefinitionVisitor (hasStopwatchExportAllVisitor comment)
        |> Review.Rule.fromModuleRuleSchema


hasStopwatchExportAllVisitor : Comment -> Node Module -> List (Error {})
hasStopwatchExportAllVisitor comment node =
    let
        hasStopwatchExposed =
            List.Extra.findMap
                (\(Node range expose) ->
                    case expose of
                        TypeExpose { name } ->
                            if name == "Stopwatch" then
                                Just range

                            else
                                Nothing

                        _ ->
                            Nothing
                )

        checkExposing exposingList =
            case Node.value exposingList of
                Exposing.All _ ->
                    []

                Exposing.Explicit exposed ->
                    case hasStopwatchExposed exposed of
                        Just range ->
                            [ Comment.createError comment range ]

                        Nothing ->
                            []
    in
    case Node.value node of
        NormalModule { exposingList } ->
            checkExposing exposingList

        PortModule { exposingList } ->
            checkExposing exposingList

        EffectModule { exposingList } ->
            checkExposing exposingList
