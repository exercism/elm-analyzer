module Exercise.BettysBikeShop exposing (hasFunctionSignatures, importString, ruleConfig)

import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/BettysBikeShop.elm" ]
    , rules =
        [ CustomRule hasFunctionSignatures (Comment "has no signature" "elm.bettys-bike-shop.use_signature" Essential Dict.empty)
        , CustomRule importString (Comment "does not import String" "elm.bettys-bike-shop.import_string" Essential Dict.empty)
        ]
    }



{- All top-level functions must use type signatures -}


hasFunctionSignatures : Comment -> Rule
hasFunctionSignatures comment =
    Rule.newModuleRuleSchema "elm.bettys-bike-shop.use_signature" []
        |> Rule.withDeclarationEnterVisitor (hasSignatureVisitor comment)
        |> Rule.withFinalModuleEvaluation hasSignatureFinalEvaluation
        |> Rule.fromModuleRuleSchema



{- Finding missing type signatures

   We add the error to a context (which is a list of errors) instead of emitting it so we can limit the number of errors to one at the end.
-}


hasSignatureVisitor : Comment -> Node Declaration -> List (Error {}) -> ( List empty, List (Error {}) )
hasSignatureVisitor comment node errors =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration, signature } ->
            case signature of
                Nothing ->
                    ( []
                    , Comment.createError comment
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


hasSignatureFinalEvaluation : List (Error {}) -> List (Error {})
hasSignatureFinalEvaluation =
    List.take 1



{- String module must be imported -}


importString : Comment -> Rule
importString comment =
    Rule.newModuleRuleSchema "elm.bettys-bike-shop.import_string" False
        |> Rule.withImportVisitor importStringVisitor
        |> Rule.withFinalModuleEvaluation (importStringFinalEvaluation comment)
        |> Rule.fromModuleRuleSchema



{- Keeping tracks of imports in the context (Bool) -}


importStringVisitor : Node Import -> Bool -> ( List empty, Bool )
importStringVisitor node importedString =
    case node |> Node.value |> .moduleName |> Node.value of
        [ "String" ] ->
            ( [], True )

        _ ->
            ( [], importedString )



{- Emit error if String was not imported -}


importStringFinalEvaluation : Comment -> Bool -> List (Error {})
importStringFinalEvaluation comment importedString =
    if importedString then
        []

    else
        [ Comment.createGlobalError comment ]
