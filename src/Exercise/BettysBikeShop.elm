module Exercise.BettysBikeShop exposing (hasFunctionSignatures, ruleConfig)

import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "bettys-bike-shop"
    , restrictToFiles = Just [ "src/BettysBikeShop.elm" ]
    , rules = [ CustomRule hasFunctionSignatures ]
    }


hasFunctionSignatures : Rule
hasFunctionSignatures =
    Rule.newModuleRuleSchema "elm.bettys-bike-shop.use_signature" []
        |> Rule.withDeclarationEnterVisitor hasSignatureVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema



{- Finding missing type signatures

   We add the error to a context (which is a list of errors) instead of emitting it so we can limit the number of errors to one at the end.
-}


hasSignatureVisitor : Node Declaration -> List (Error {}) -> ( List empty, List (Error {}) )
hasSignatureVisitor node errors =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration, signature } ->
            case signature of
                Nothing ->
                    ( []
                    , Comment.createError
                        (Comment "has no signature" "elm.bettys-bike-shop.use_signature" Essential Dict.empty)
                        (declaration |> Node.value |> .name |> Node.range)
                        :: errors
                    )

                Just _ ->
                    ( [], errors )

        _ ->
            ( [], errors )



{- Emitting one error

   We only emit one error at the end to have a simple message "Please add type signatures to all top-level functions"
-}


finalEvaluation : List (Error {}) -> List (Error {})
finalEvaluation =
    List.take 1
