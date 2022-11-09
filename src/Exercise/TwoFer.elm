module Exercise.TwoFer exposing (hasFunctionSignature, ruleConfig, usesWithDefault)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "two-fer"
    , restrictToFiles = Just [ "src/TwoFer.elm" ]
    , rules =
        [ CustomRule hasFunctionSignature (Comment "has no signature" "elm.two-fer.use_signature" Informative Dict.empty)
        , CustomRule usesWithDefault (Comment "Doesn't use withDefault" "elm.two-fer.use_withDefault" Informative Dict.empty)
        ]
    }


hasFunctionSignature : Comment -> Rule
hasFunctionSignature comment =
    Rule.newModuleRuleSchema "elm.two-fer.use_signature" ()
        |> Rule.withSimpleDeclarationVisitor (hasSignatureVisitor comment)
        |> Rule.fromModuleRuleSchema


hasSignatureVisitor : Comment -> Node Declaration -> List (Error {})
hasSignatureVisitor comment node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration, signature } ->
            case signature of
                Nothing ->
                    [ Comment.createError
                        comment
                        (declaration |> Node.value |> .name |> Node.range)
                    ]

                Just _ ->
                    []

        _ ->
            []


usesWithDefault : Comment -> Rule
usesWithDefault =
    Analyzer.functionCalls
        { calledFrom = TopFunction "twoFer"
        , findFunctions = [ FromExternalModule [ "Maybe" ] "withDefault" ]
        , find = All
        }
