module Tags exposing (commonTagsRule, expressionTagsRule, ruleConfig)

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import ElmSyntaxHelpers
import Json.Encode as Encode exposing (Value)
import Review.ModuleNameLookupTable as LookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)
import Set exposing (Set)


type alias Tag =
    String


type alias ProjectContext =
    { tags : Set Tag
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , tags : Set Tag
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
    Rule.newProjectRuleSchema "Tags" commonTagsProjectContext
        |> Rule.withModuleVisitor (Rule.withSimpleModuleDefinitionVisitor (always []))
        |> Rule.withModuleContextUsingContextCreator
            { fromModuleToProject = fromModuleToProject
            , fromProjectToModule = fromProjectToModule
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


commonTagsProjectContext : ProjectContext
commonTagsProjectContext =
    ProjectContext
        (Set.fromList
            [ "paradigm:functional"
            , "technique:immutability"
            , "uses:module"
            ]
        )


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
    Rule.initContextCreator (\{ tags } -> ProjectContext tags)


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts a b =
    ProjectContext (Set.union a.tags b.tags)


dataExtractor : ProjectContext -> Value
dataExtractor =
    .tags >> Set.toList >> Encode.list Encode.string


expressionTagsRule : Rule
expressionTagsRule =
    Rule.newProjectRuleSchema "Tags" emptyProjectContext
        |> Rule.withModuleVisitor (Rule.withExpressionEnterVisitor expressionVisitor)
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


expressionVisitor : Node Expression -> ModuleContext -> ( List never, ModuleContext )
expressionVisitor ((Node range expression) as node) ({ lookupTable, tags } as context) =
    case expression of
        FunctionOrValue _ name ->
            case LookupTable.moduleNameFor lookupTable node of
                Nothing ->
                    ( [], { context | tags = Set.union tags (matchExpression node) } )

                Just originalModuleName ->
                    ( [], { context | tags = Set.union tags (matchExpression (Node range (FunctionOrValue originalModuleName name))) } )

        _ ->
            ( [], { context | tags = Set.union tags (matchExpression node) } )


matchExpression : Node Expression -> Set Tag
matchExpression (Node range expression) =
    case expression of
        FunctionOrValue [ "Set" ] _ ->
            Set.fromList [ "construct:set", "technique:immutable-collection", "technique:sorted-collection" ]

        FunctionOrValue [ "Dict" ] _ ->
            Set.fromList [ "construct:dictionary", "technique:immutable-collection", "technique:sorted-collection" ]

        FunctionOrValue [ "Random" ] _ ->
            Set.singleton "technique:randomness"

        FunctionOrValue [ "Regex" ] _ ->
            Set.singleton "technique:regular-expression"

        FunctionOrValue [ "Debug" ] _ ->
            Set.singleton "uses:debug"

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

        LetExpression { declarations } ->
            if List.any (Node.value >> usesProperDestructuring) declarations then
                Set.fromList [ "uses:destructure", "construct:pattern-matching" ]

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
