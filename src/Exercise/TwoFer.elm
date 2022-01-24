module Exercise.TwoFer exposing (hasFunctionSignature, usesWithDefault)

import Analyzer exposing (CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


slug : String
slug =
    "two-fer"


hasFunctionSignature : Rule
hasFunctionSignature =
    Rule.newModuleRuleSchema slug ()
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
        { calledFrom = Just "twoFer"
        , findFunctions = [ FromExternalModule [ "Maybe" ] "withDefault" ]
        , find = All
        , comment = Comment "Doesn't use withDefault" "elm.two-fer.use_withDefault" Essential Dict.empty
        }
