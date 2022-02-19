module Exercise.TwoFer exposing (hasFunctionSignature, ruleConfig, usesWithDefault)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import RuleConfig exposing (RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "two-fer"
    , restrictToFiles = Just [ "src/TwoFer.elm" ]
    , rules = [ hasFunctionSignature, usesWithDefault ]
    , highjackErrorDecoders = []
    }


hasFunctionSignature : Rule
hasFunctionSignature =
    Rule.newModuleRuleSchema "elm.two-fer.use_signature" ()
        |> Rule.withSimpleDeclarationVisitor hasSignatureVisitor
        |> Rule.fromModuleRuleSchema


hasSignatureVisitor : Node Declaration -> List (Error {})
hasSignatureVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration, signature } ->
            case signature of
                Nothing ->
                    [ Comment.createError
                        (Comment "has no signature" "elm.two-fer.use_signature" Informative Dict.empty)
                        (declaration |> Node.value |> .name |> Node.range)
                    ]

                Just _ ->
                    []

        _ ->
            []


usesWithDefault : Rule
usesWithDefault =
    Analyzer.functionCalls
        { calledFrom = TopFunction "twoFer"
        , findFunctions = [ FromExternalModule [ "Maybe" ] "withDefault" ]
        , find = All
        , comment = Comment "Doesn't use withDefault" "elm.two-fer.use_withDefault" Informative Dict.empty
        }
