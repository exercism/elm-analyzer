module Exercise.ZebraPuzzle exposing (hardcodingDrinksWater, hardcodingOwnsZebra, ruleConfig)

import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/ZebraPuzzle.elm" ]
    , rules =
        [ CustomRule hardcodingDrinksWater
            (Comment "elm.zebra-puzzle.do_not_hardcode_solution" Essential Dict.empty)
        , CustomRule hardcodingOwnsZebra
            (Comment "elm.zebra-puzzle.do_not_hardcode_solution" Essential Dict.empty)
        ]
    }


hardcodingDrinksWater : Comment -> Rule
hardcodingDrinksWater comment =
    Rule.newModuleRuleSchema comment.path []
        |> Rule.withSimpleDeclarationVisitor (hardcodedDeclaration comment "drinksWater")
        |> Rule.fromModuleRuleSchema


hardcodingOwnsZebra : Comment -> Rule
hardcodingOwnsZebra comment =
    Rule.newModuleRuleSchema "elm.zebra-puzzle.do_not_hardcode_solution" []
        |> Rule.withSimpleDeclarationVisitor (hardcodedDeclaration comment "ownsZebra")
        |> Rule.fromModuleRuleSchema


hardcodedDeclaration : Comment -> String -> Node Declaration -> List (Rule.Error {})
hardcodedDeclaration comment functionName node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            if (declaration |> Node.value |> .name |> Node.value) == functionName then
                case declaration |> Node.value |> .expression |> Node.value of
                    Application [ Node _ (FunctionOrValue _ "Just"), Node _ (FunctionOrValue _ _) ] ->
                        [ Comment.createError comment (declaration |> Node.value |> .name |> Node.range) ]

                    _ ->
                        []

            else
                []

        _ ->
            []
