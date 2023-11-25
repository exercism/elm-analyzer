module Tags exposing (commonTagsRule, expressionTagsRule, ruleConfig)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import ElmSyntaxHelpers
import Json.Encode as Encode exposing (Value)
import Review.ModuleNameLookupTable as LookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)
import Set exposing (Set)


type alias ProjectContext =
    { tags : Set String
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , tags : Set String
    }


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Nothing
    , rules =
        [ TagRule commonTagsRule
        , TagRule expressionTagsRule
        ]
    }


commonTagsRule : Rule
commonTagsRule =
    Rule.newProjectRuleSchema "commonTags" emptyProjectContext
        |> Rule.withModuleVisitor (Rule.withModuleDefinitionVisitor commonModuleVisitor)
        |> Rule.withModuleContextUsingContextCreator
            { fromModuleToProject = fromModuleToProject
            , fromProjectToModule = fromProjectToModule
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


expressionTagsRule : Rule
expressionTagsRule =
    Rule.newProjectRuleSchema "expressionTags" emptyProjectContext
        |> Rule.withModuleVisitor
            (Rule.withDeclarationEnterVisitor declarationVisitor
                >> Rule.withExpressionEnterVisitor expressionVisitor
                >> Rule.withModuleDocumentationVisitor documentationVisitor
                >> Rule.withCommentsVisitor commentsVisitor
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromModuleToProject = fromModuleToProject
            , fromProjectToModule = fromProjectToModule
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


emptyProjectContext : ProjectContext
emptyProjectContext =
    ProjectContext Set.empty


commonModuleVisitor : Node Module -> ModuleContext -> ( List never, ModuleContext )
commonModuleVisitor _ context =
    ( [], { context | tags = commonTags } )


commonTags : Set String
commonTags =
    Set.fromList
        [ "paradigm:functional"
        , "technique:immutability"
        , "uses:module"
        ]


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable _ ->
            { lookupTable = lookupTable
            , tags = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\isFileIgnored { tags } ->
            if isFileIgnored then
                emptyProjectContext

            else
                ProjectContext tags
        )
        |> Rule.withIsFileIgnored


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts a b =
    ProjectContext (Set.union a.tags b.tags)


dataExtractor : ProjectContext -> Value
dataExtractor =
    .tags >> Set.toList >> Encode.list Encode.string


declarationVisitor : Node Declaration -> ModuleContext -> ( List never, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        FunctionDeclaration { documentation } ->
            documentationVisitor documentation context

        _ ->
            ( [], context )


documentationVisitor : Maybe documentation -> ModuleContext -> ( List never, ModuleContext )
documentationVisitor maybeDoc ({ tags } as context) =
    let
        docTags =
            Set.fromList [ "construct:comment", "construct:documentation" ]
    in
    case maybeDoc of
        Nothing ->
            ( [], context )

        Just _ ->
            ( [], { context | tags = Set.union docTags tags } )


commentsVisitor : List (Node String) -> ModuleContext -> ( List never, ModuleContext )
commentsVisitor comments ({ tags } as context) =
    case comments of
        [] ->
            ( [], context )

        _ ->
            ( [], { context | tags = Set.insert "construct:comment" tags } )


expressionVisitor : Node Expression -> ModuleContext -> ( List never, ModuleContext )
expressionVisitor ((Node range expression) as node) ({ lookupTable, tags } as context) =
    let
        matches n =
            Set.union (matchExpressionType n) (matchExpression n)
    in
    case expression of
        FunctionOrValue _ name ->
            case LookupTable.moduleNameFor lookupTable node of
                Nothing ->
                    ( [], { context | tags = Set.union tags (matches node) } )

                Just originalModuleName ->
                    ( [], { context | tags = Set.union tags (matches (Node range (FunctionOrValue originalModuleName name))) } )

        _ ->
            ( [], { context | tags = Set.union tags (matches node) } )


{-| Only looks at the type of the expression, not the content
-}
matchExpressionType : Node Expression -> Set String
matchExpressionType (Node range expression) =
    case expression of
        Application _ ->
            Set.singleton "uses:function-application"

        OperatorApplication _ _ _ _ ->
            Set.singleton "uses:function-application"

        PrefixOperator _ ->
            Set.singleton "uses:prefix-operator"

        UnitExpr ->
            Set.singleton "uses:unit"

        Floatable _ ->
            Set.fromList [ "construct:float", "construct:floating-point-number" ]

        Integer _ ->
            Set.fromList [ "construct:integral-number", "construct:int" ]

        Hex _ ->
            Set.fromList [ "construct:hexadecimal-number", "construct:integral-number", "construct:int" ]

        Negation _ ->
            Set.singleton "construct:unary-minus"

        Literal _ ->
            if range.end.row > range.start.row then
                Set.fromList [ "construct:string", "construct:multiline-string" ]

            else
                Set.singleton "construct:string"

        LambdaExpression _ ->
            Set.singleton "construct:lambda"

        IfBlock _ _ _ ->
            Set.fromList [ "construct:if", "construct:boolean" ]

        LetExpression _ ->
            Set.singleton "construct:assignment"

        CharLiteral _ ->
            Set.singleton "construct:char"

        TupledExpression _ ->
            Set.singleton "construct:tuple"

        CaseExpression _ ->
            Set.singleton "construct:pattern-matching"

        RecordExpr _ ->
            Set.singleton "construct:record"

        RecordAccess _ _ ->
            Set.fromList [ "construct:record", "uses:record-access" ]

        RecordAccessFunction _ ->
            Set.fromList [ "construct:record", "uses:record-access", "uses:record-access-function" ]

        RecordUpdateExpression _ _ ->
            Set.fromList [ "construct:record", "uses:record-update" ]

        ListExpr _ ->
            Set.singleton "construct:list"

        GLSLExpression _ ->
            Set.singleton "uses:glsl"

        FunctionOrValue _ value ->
            case String.uncons value of
                Nothing ->
                    Set.empty

                Just ( first, _ ) ->
                    if Char.isUpper first then
                        Set.singleton "construct:constructor"

                    else
                        Set.empty

        ParenthesizedExpression _ ->
            Set.empty

        -- not possible to get in practice
        Operator _ ->
            Set.empty


matchExpression : Node Expression -> Set String
matchExpression (Node _ expression) =
    case expression of
        FunctionOrValue [ "Bitwise" ] "and" ->
            Set.fromList [ "construct:bit-manipulation", "construct:bitwise-and" ]

        FunctionOrValue [ "Bitwise" ] "or" ->
            Set.fromList [ "construct:bit-manipulation", "construct:bitwise-or" ]

        FunctionOrValue [ "Bitwise" ] "xor" ->
            Set.fromList [ "construct:bit-manipulation", "construct:bitwise-xor" ]

        FunctionOrValue [ "Bitwise" ] "complement" ->
            Set.fromList [ "construct:bit-manipulation", "construct:bitwise-not" ]

        FunctionOrValue [ "Bitwise" ] "shiftLeftBy" ->
            Set.fromList [ "construct:bit-manipulation", "technique:bit-shifting", "construct:bitwise-left-shift" ]

        FunctionOrValue [ "Bitwise" ] "shiftRightBy" ->
            Set.fromList [ "construct:bit-manipulation", "technique:bit-shifting", "construct:bitwise-right-shift" ]

        FunctionOrValue [ "Bitwise" ] "shiftRightZfBy" ->
            Set.fromList [ "construct:bit-manipulation", "technique:bit-shifting" ]

        FunctionOrValue [ "Array" ] _ ->
            Set.fromList [ "construct:array", "technique:immutable-collection" ]

        FunctionOrValue ("Bytes" :: _) _ ->
            Set.singleton "construct:byte"

        FunctionOrValue [ "Set" ] _ ->
            Set.fromList [ "construct:set", "technique:immutable-collection", "technique:sorted-collection" ]

        FunctionOrValue [ "Dict" ] _ ->
            Set.fromList [ "construct:dictionary", "technique:immutable-collection", "technique:sorted-collection" ]

        FunctionOrValue [ "List" ] _ ->
            Set.singleton "construct:list"

        FunctionOrValue [ "Random" ] _ ->
            Set.singleton "technique:randomness"

        FunctionOrValue [ "Regex" ] _ ->
            Set.singleton "technique:regular-expression"

        FunctionOrValue [ "Debug" ] _ ->
            Set.singleton "uses:debug"

        PrefixOperator "&&" ->
            Set.fromList [ "construct:boolean", "construct:logical-and", "technique:boolean-logic" ]

        OperatorApplication "&&" _ _ _ ->
            Set.fromList [ "construct:boolean", "construct:logical-and", "technique:boolean-logic" ]

        PrefixOperator "||" ->
            Set.fromList [ "construct:boolean", "construct:logical-or", "technique:boolean-logic" ]

        OperatorApplication "||" _ _ _ ->
            Set.fromList [ "construct:boolean", "construct:logical-or", "technique:boolean-logic" ]

        FunctionOrValue [ "Basics" ] "not" ->
            Set.fromList [ "construct:boolean", "construct:logical-not", "technique:boolean-logic" ]

        FunctionOrValue [ "Basics" ] "xor" ->
            Set.fromList [ "construct:boolean", "technique:boolean-logic" ]

        FunctionOrValue [ "Basics" ] "True" ->
            Set.singleton "construct:boolean"

        FunctionOrValue [ "Basics" ] "False" ->
            Set.singleton "construct:boolean"

        FunctionOrValue [ "Basics" ] "isNaN" ->
            Set.fromList [ "construct:boolean", "construct:float", "construct:floating-point-number" ]

        FunctionOrValue [ "Basics" ] "isInfinite" ->
            Set.fromList [ "construct:boolean", "construct:float", "construct:floating-point-number" ]

        PrefixOperator "+" ->
            Set.singleton "construct:add"

        OperatorApplication "+" _ _ _ ->
            Set.singleton "construct:add"

        PrefixOperator "-" ->
            Set.singleton "construct:subtract"

        OperatorApplication "-" _ _ _ ->
            Set.singleton "construct:subtract"

        PrefixOperator "*" ->
            Set.singleton "construct:multiply"

        OperatorApplication "*" _ _ _ ->
            Set.singleton "construct:multiply"

        PrefixOperator "/" ->
            Set.singleton "construct:divide"

        OperatorApplication "/" _ _ _ ->
            Set.singleton "construct:divide"

        PrefixOperator "//" ->
            Set.singleton "construct:divide"

        OperatorApplication "//" _ _ _ ->
            Set.singleton "construct:divide"

        PrefixOperator "==" ->
            Set.fromList [ "construct:equality", "technique:equality-comparison", "construct:boolean" ]

        OperatorApplication "==" _ _ _ ->
            Set.fromList [ "construct:equality", "technique:equality-comparison", "construct:boolean" ]

        PrefixOperator "/=" ->
            Set.fromList [ "construct:inequality", "technique:equality-comparison", "construct:boolean" ]

        OperatorApplication "/=" _ _ _ ->
            Set.fromList [ "construct:inequality", "technique:equality-comparison", "construct:boolean" ]

        LetExpression { declarations } ->
            if List.any (Node.value >> usesProperDestructuring) declarations then
                Set.fromList [ "construct:destructuring", "construct:pattern-matching" ]

            else
                Set.empty

        _ ->
            Set.empty


usesProperDestructuring : LetDeclaration -> Bool
usesProperDestructuring letDeclaration =
    case letDeclaration of
        LetDestructuring _ _ ->
            True

        LetFunction { declaration } ->
            declaration
                |> Node.value
                |> .arguments
                |> List.any ElmSyntaxHelpers.hasDestructuringPattern
