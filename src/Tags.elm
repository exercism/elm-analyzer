module Tags exposing (commonTagsRule, expressionTagsRule, ruleConfig)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import ElmSyntaxHelpers exposing (hasAnythingGeneric)
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
    , exposedFunctions : ExposedFunctions
    , inFunction : Maybe String
    , tags : Set String
    }


type ExposedFunctions
    = All
    | Some (List String)


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
            (Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
                >> Rule.withDeclarationEnterVisitor declarationEnterVisitor
                >> Rule.withDeclarationExitVisitor declarationExitVisitor
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
        , "construct:type-inference"
        , "technique:immutability"
        , "uses:module"
        ]


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable _ ->
            { lookupTable = lookupTable
            , exposedFunctions = All
            , inFunction = Nothing
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


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List never, ModuleContext )
moduleDefinitionVisitor (Node _ moduleNode) context =
    case moduleNode of
        PortModule _ ->
            ( [], context )

        EffectModule _ ->
            ( [], context )

        NormalModule { exposingList } ->
            case Node.value exposingList of
                Exposing.All _ ->
                    ( [], { context | exposedFunctions = All } )

                Exposing.Explicit exposed ->
                    let
                        getExposedFunctions expose =
                            case Node.value expose of
                                FunctionExpose name ->
                                    Just name

                                _ ->
                                    Nothing

                        list =
                            List.filterMap getExposedFunctions exposed
                    in
                    ( [], { context | exposedFunctions = Some list } )


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List never, ModuleContext )
declarationEnterVisitor node ({ tags } as context) =
    case Node.value node of
        FunctionDeclaration { documentation, declaration, signature } ->
            let
                ( _, docContext ) =
                    documentationVisitor documentation context

                functionImplTags =
                    functionImplementationTags declaration context

                signTags =
                    case signature of
                        Nothing ->
                            Set.empty

                        Just sign ->
                            signatureTags sign

                newTags =
                    tags
                        |> Set.union docContext.tags
                        |> Set.union functionImplTags
                        |> Set.union signTags

                functionName =
                    declaration |> Node.value |> .name |> Node.value
            in
            ( [], { context | tags = newTags, inFunction = Just functionName } )

        AliasDeclaration _ ->
            ( [], { context | tags = Set.insert "uses:type-alias" tags } )

        CustomTypeDeclaration customType ->
            ( [], { context | tags = Set.union (analyzeCustomType customType) tags } )

        _ ->
            ( [], context )


declarationExitVisitor : Node Declaration -> ModuleContext -> ( List never, ModuleContext )
declarationExitVisitor _ context =
    ( [], { context | inFunction = Nothing } )


signatureTags : Node Signature -> Set String
signatureTags (Node _ { typeAnnotation }) =
    if hasAnythingGeneric typeAnnotation then
        Set.singleton "construct:generic-type"

    else
        Set.empty


analyzeCustomType : Type -> Set String
analyzeCustomType { name, generics, constructors } =
    let
        unionTags =
            case constructors of
                _ :: _ :: _ ->
                    [ "uses:union-type" ]

                _ ->
                    []

        genericsTags =
            case generics of
                [] ->
                    []

                _ ->
                    [ "construct:generic-type" ]

        isRecursive =
            constructors
                |> List.concatMap (Node.value >> .arguments)
                |> List.any (ElmSyntaxHelpers.hasTyped [] (Node.value name))

        recursiveTags =
            if isRecursive then
                [ "construct:recursive-type" ]

            else
                []
    in
    Set.fromList ("uses:custom-type" :: genericsTags ++ unionTags ++ recursiveTags)


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


functionImplementationTags : Node FunctionImplementation -> ModuleContext -> Set String
functionImplementationTags (Node _ { name, arguments }) { exposedFunctions } =
    let
        argTags =
            if List.any ElmSyntaxHelpers.hasDestructuringPattern arguments then
                Set.fromList [ "construct:destructuring", "construct:pattern-matching" ]

            else
                Set.empty

        nameTags =
            case exposedFunctions of
                All ->
                    Set.empty

                Some names ->
                    if List.member (Node.value name) names then
                        Set.empty

                    else
                        Set.singleton "construct:local-function"
    in
    Set.union argTags nameTags


expressionVisitor : Node Expression -> ModuleContext -> ( List never, ModuleContext )
expressionVisitor ((Node range expression) as node) ({ lookupTable, tags, inFunction } as context) =
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
                    let
                        expressionTags =
                            Set.union tags (matches (Node range (FunctionOrValue originalModuleName name)))

                        recursionTag =
                            if originalModuleName == [] && inFunction == Just name then
                                Set.singleton "technique:recursion"

                            else
                                Set.empty
                    in
                    ( [], { context | tags = Set.union expressionTags recursionTag } )

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
            Set.fromList [ "construct:pattern-matching", "construct:switch" ]

        RecordExpr _ ->
            Set.singleton "construct:record"

        RecordAccess _ _ ->
            Set.fromList [ "construct:record", "uses:record-access" ]

        RecordAccessFunction _ ->
            Set.fromList [ "construct:record", "uses:record-access", "uses:record-access-function" ]

        RecordUpdateExpression _ _ ->
            Set.fromList [ "construct:record", "uses:record-update" ]

        ListExpr _ ->
            Set.fromList [ "construct:linked-list", "construct:list" ]

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

        FunctionOrValue [ "String" ] _ ->
            Set.singleton "construct:string"

        FunctionOrValue [ "Array" ] _ ->
            Set.fromList [ "construct:array", "technique:immutable-collection" ]

        FunctionOrValue ("Bytes" :: _) _ ->
            Set.singleton "construct:byte"

        FunctionOrValue [ "Set" ] _ ->
            Set.fromList [ "construct:set", "technique:immutable-collection", "technique:sorted-collection" ]

        FunctionOrValue [ "Time" ] _ ->
            Set.singleton "construct:date-time"

        FunctionOrValue [ "Dict" ] _ ->
            Set.fromList [ "construct:dictionary", "technique:immutable-collection", "technique:sorted-collection" ]

        FunctionOrValue [ "List" ] function ->
            let
                common =
                    Set.fromList [ "construct:linked-list", "construct:list", "technique:immutable-collection" ]

                special =
                    case function of
                        "sortWith" ->
                            Set.singleton "technique:ordering"

                        _ ->
                            Set.empty
            in
            Set.union common special

        FunctionOrValue [ "Random" ] _ ->
            Set.singleton "technique:randomness"

        FunctionOrValue [ "Regex" ] _ ->
            Set.singleton "technique:regular-expression"

        FunctionOrValue [ "Debug" ] _ ->
            Set.singleton "uses:debug"

        PrefixOperator "&&" ->
            Set.fromList [ "construct:boolean", "construct:logical-and", "technique:boolean-logic", "technique:short-circuiting" ]

        OperatorApplication "&&" _ _ _ ->
            Set.fromList [ "construct:boolean", "construct:logical-and", "technique:boolean-logic", "technique:short-circuiting" ]

        PrefixOperator "||" ->
            Set.fromList [ "construct:boolean", "construct:logical-or", "technique:boolean-logic", "technique:short-circuiting" ]

        OperatorApplication "||" _ _ _ ->
            Set.fromList [ "construct:boolean", "construct:logical-or", "technique:boolean-logic", "technique:short-circuiting" ]

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

        FunctionOrValue [ "Basics" ] "modBy" ->
            Set.fromList [ "construct:integral-number", "construct:int", "construct:modulo" ]

        FunctionOrValue [ "Basics" ] "remainderBy" ->
            Set.fromList [ "construct:integral-number", "construct:int", "construct:modulo" ]

        FunctionOrValue [ "Basics" ] "compare" ->
            Set.singleton "technique:ordering"

        FunctionOrValue [ "Basics" ] "LT" ->
            Set.singleton "technique:ordering"

        FunctionOrValue [ "Basics" ] "EQ" ->
            Set.singleton "technique:ordering"

        FunctionOrValue [ "Basics" ] "GT" ->
            Set.singleton "technique:ordering"

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
            if List.any (Node.value >> letUsesDestructuring) declarations then
                Set.fromList [ "construct:destructuring", "construct:pattern-matching" ]

            else
                Set.empty

        LambdaExpression { args } ->
            if List.any ElmSyntaxHelpers.hasDestructuringPattern args then
                Set.fromList [ "construct:destructuring", "construct:pattern-matching" ]

            else
                Set.empty

        CaseExpression { cases } ->
            if List.any (Tuple.first >> ElmSyntaxHelpers.hasDestructuringPattern) cases then
                Set.fromList [ "construct:destructuring", "construct:pattern-matching" ]

            else
                Set.empty

        _ ->
            Set.empty


letUsesDestructuring : LetDeclaration -> Bool
letUsesDestructuring letDeclaration =
    case letDeclaration of
        LetDestructuring _ _ ->
            True

        LetFunction { declaration } ->
            declaration
                |> Node.value
                |> .arguments
                |> List.any ElmSyntaxHelpers.hasDestructuringPattern
