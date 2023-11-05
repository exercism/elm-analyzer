module Exercise.Bandwagoner exposing (replaceCoachUsesRecordUpdateSyntax, rootForTeamHasExtensibleRecordSignature, rootForTeamUsesPatternMatchingInArgument, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..), Pattern(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import ElmSyntaxHelpers
import Review.Rule as Rule exposing (Error, Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "bandwagoner"
    , restrictToFiles = Just [ "src/Bandwagoner.elm" ]
    , rules =
        [ CustomRule replaceCoachUsesRecordUpdateSyntax
            (Comment "replaceCoach doesn't use record update syntax" "elm.bandwagoner.use_record_update_syntax" Actionable Dict.empty)
        , CustomRule rootForTeamHasExtensibleRecordSignature
            (Comment "rootForTeam has no extensible record" "elm.bandwagoner.use_extensible_record_signature" Essential Dict.empty)
        , CustomRule rootForTeamUsesPatternMatchingInArgument
            (Comment "rootForTeam doesn't use pattern matching in argument" "elm.bandwagoner.use_pattern_matching_in_argument" Essential Dict.empty)
        ]
    }


replaceCoachUsesRecordUpdateSyntax : Comment -> Rule
replaceCoachUsesRecordUpdateSyntax =
    Analyzer.functionCalls
        { calledFrom = TopFunction "replaceCoach"
        , findExpressions = [ RecordUpdate ]
        , find = Some
        }


rootForTeamUsesPatternMatchingInArgument : Comment -> Rule
rootForTeamUsesPatternMatchingInArgument =
    Analyzer.functionCalls
        { calledFrom = TopFunction "rootForTeam"
        , findExpressions = [ ArgumentWithPattern Record ]
        , find = Some
        }


rootForTeamHasExtensibleRecordSignature : Comment -> Rule
rootForTeamHasExtensibleRecordSignature comment =
    Rule.newModuleRuleSchema comment.path []
        |> Rule.withSimpleDeclarationVisitor (hasExtensibleRecordSignatureVisitor comment "rootForTeam")
        |> Rule.fromModuleRuleSchema


hasExtensibleRecordSignatureVisitor : Comment -> String -> Node Declaration -> List (Error {})
hasExtensibleRecordSignatureVisitor comment functionName node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration, signature } ->
            let
                error =
                    [ Comment.createError comment (declaration |> Node.value |> .name |> Node.range) ]
            in
            if (declaration |> Node.value |> .name |> Node.value) == functionName then
                case signature of
                    Nothing ->
                        error

                    Just (Node _ { typeAnnotation }) ->
                        if ElmSyntaxHelpers.hasGenericRecord typeAnnotation then
                            []

                        else
                            error

            else
                []

        _ ->
            []
