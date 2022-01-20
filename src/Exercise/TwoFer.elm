module Exercise.TwoFer exposing (hasFunctionSignature, usesWithDefault)

import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.ModuleNameLookupTable as LookupTable exposing (ModuleNameLookupTable)
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
        Declaration.FunctionDeclaration { signature } ->
            case signature of
                Nothing ->
                    [ Comment.createError
                        (Comment "has no signature" "elm.two-fer.use_signature" Informative Dict.empty)
                        (Node.range node)
                    ]

                Just _ ->
                    []

        _ ->
            []


type alias UsesWithDefaultContext =
    { isFunctionDeclaration : Bool
    , usesWithDefault : Bool
    , lookupTable : ModuleNameLookupTable
    }


initialUsesWithDefaultContext : Rule.ContextCreator () UsesWithDefaultContext
initialUsesWithDefaultContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , isFunctionDeclaration = False
            , usesWithDefault = False
            }
        )
        |> Rule.withModuleNameLookupTable


usesWithDefault : Rule
usesWithDefault =
    Rule.newModuleRuleSchemaUsingContextCreator slug initialUsesWithDefaultContext
        |> Rule.withDeclarationEnterVisitor annotateFunctionDeclaration
        |> Rule.withExpressionEnterVisitor expressionUsesWithDefault
        |> Rule.withDeclarationExitVisitor checkUsedWithDefault
        |> Rule.fromModuleRuleSchema


annotateFunctionDeclaration : Node Declaration -> UsesWithDefaultContext -> ( List (Error {}), UsesWithDefaultContext )
annotateFunctionDeclaration node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            ( [], { context | isFunctionDeclaration = True } )

        _ ->
            ( [], context )


expressionUsesWithDefault : Node Expression -> UsesWithDefaultContext -> ( List (Error {}), UsesWithDefaultContext )
expressionUsesWithDefault expression ({ isFunctionDeclaration, lookupTable } as context) =
    case Node.value expression of
        Expression.FunctionOrValue _ "withDefault" ->
            if isFunctionDeclaration && LookupTable.moduleNameFor lookupTable expression == Just [ "Maybe" ] then
                ( [], { context | usesWithDefault = True } )

            else
                ( [], context )

        _ ->
            ( [], context )


checkUsedWithDefault : Node Declaration -> UsesWithDefaultContext -> ( List (Error {}), UsesWithDefaultContext )
checkUsedWithDefault node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                errors =
                    if context.usesWithDefault then
                        []

                    else
                        [ Comment.createError
                            (Comment "Doesn't use withDefault" "elm.two-fer.use_withDefault" Essential Dict.empty)
                            (Node.range node)
                        ]
            in
            ( errors, { context | isFunctionDeclaration = False, usesWithDefault = False } )

        _ ->
            ( [], context )



{-
   a =
       Node { end = { column = 27, row = 8 }, start = { column = 1, row = 4 } }
           (FunctionDeclaration
               { declaration =
                   Node
                       { end = { column = 27, row = 8 }
                       , start = { column = 1, row = 5 }
                       }
                       { arguments = [ Node { end = { column = 12, row = 5 }, start = { column = 8, row = 5 } } (VarPattern "name") ]
                       , expression =
                           Node { end = { column = 27, row = 8 }, start = { column = 5, row = 6 } }
                               (OperatorApplication "++"
                                   Right
                                   (Node { end = { column = 15, row = 6 }, start = { column = 5, row = 6 } } (Literal "One for "))
                                   (Node { end = { column = 27, row = 8 }, start = { column = 12, row = 7 } }
                                       (OperatorApplication "++"
                                           Right
                                           (Node { end = { column = 40, row = 7 }, start = { column = 12, row = 7 } }
                                               (Application
                                                   [ Node
                                                       { end = { column = 29, row = 7 }
                                                       , start = { column = 12, row = 7 }
                                                       }
                                                       (FunctionOrValue [ "Maybe" ] "withDefault")
                                                   , Node { end = { column = 35, row = 7 }, start = { column = 30, row = 7 } } (Literal "you")
                                                   , Node { end = { column = 40, row = 7 }, start = { column = 36, row = 7 } } (FunctionOrValue [] "name")
                                                   ]
                                               )
                                           )
                                           (Node { end = { column = 27, row = 8 }, start = { column = 12, row = 8 } } (Literal ", one for me."))
                                       )
                                   )
                               )
                       , name = Node { end = { column = 7, row = 5 }, start = { column = 1, row = 5 } } "twoFer"
                       }
               , documentation = Nothing
               , signature =
                   Just
                       (Node { end = { column = 32, row = 4 }, start = { column = 1, row = 4 } }
                           { name = Node { end = { column = 7, row = 4 }, start = { column = 1, row = 4 } } "twoFer"
                           , typeAnnotation =
                               Node
                                   { end = { column = 32, row = 4 }
                                   , start = { column = 10, row = 4 }
                                   }
                                   (FunctionTypeAnnotation
                                       (Node { end = { column = 22, row = 4 }, start = { column = 10, row = 4 } }
                                           (Typed
                                               (Node
                                                   { end = { column = 15, row = 4 }
                                                   , start = { column = 10, row = 4 }
                                                   }
                                                   ( [], "Maybe" )
                                               )
                                               [ Node { end = { column = 22, row = 4 }, start = { column = 16, row = 4 } } (Typed (Node { end = { column = 22, row = 4 }, start = { column = 16, row = 4 } } ( [], "String" )) []) ]
                                           )
                                       )
                                       (Node { end = { column = 32, row = 4 }, start = { column = 26, row = 4 } } (Typed (Node { end = { column = 32, row = 4 }, start = { column = 26, row = 4 } } ( [], "String" )) []))
                                   )
                           }
                       )
               }
           )
-}
