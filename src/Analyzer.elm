module Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..), Pattern(..), functionCalls)

import Comment exposing (Comment)
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import ElmSyntaxHelpers
import Review.ModuleNameLookupTable as LookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


type alias FunctionName =
    String


{-| Specifies where the functions are called from.

`Anywhere` means the functions are searched through the whole module.

`TopFunction "functionA"` means the functions are searched only within `functionA` and the functions called by `functionA`.

-}
type CalledFrom
    = Anywhere
    | TopFunction FunctionName


{-| Expressions that can be searched for.

`AnyFromExternalModule ["List", "Extra"]` means that we are looking for any function from the `List.Extra` module.

`FromExternalModule ["List", "Extra"] "find"` means that we a looking for a call to `List.Extra.find`.

Note that for searching for function that are automatically imported, like `round`, the module must be explicitely stated: `FromExternalModule ["Basics"] "round"`.

`FromSameModule "solve"` means that we are looking for a call to a top-level function defined in the same module.

`LetBlock` means looking for a `let` expression.

`CaseBlock` means looking for a `case` expression.

`RecordUpdate` meanls looking for a record update expression.

`Operator "::"` means we are looking for the operator either applied `head :: tail` or standalone `List.map2 (::)`.

`ArgumentWithPattern pattern` means we are looking for a pattern match in a top-level function argument.

`LetWithPattern pattern`, `CaseWithPattern pattern`, `LambdaWithPattern pattern`, means we are looking for a destructure pattern in a let, case, or lambda function expression.

-}



-- TODO: also search for if, or any expression?


type CalledExpression
    = AnyFromExternalModule ModuleName
    | FromExternalModule ModuleName FunctionName
    | FromSameModule FunctionName
    | LetBlock
    | CaseBlock
    | RecordUpdate
    | Operator FunctionName
    | ArgumentWithPattern Pattern
    | LetWithPattern Pattern
    | CaseWithPattern Pattern
    | LambdaWithPattern Pattern


{-| Patterns that can be searched for, either in function arguments or in expressions that support them.

`Record` is for records: `let {a, b} = rec in ...`.

`Tuple` is for tuples: `let (a, b) = pair in ...`.

`Ignore` is for the wild card: `let _ = ignored in ...`.

`Named` is for constructors: `let Just x = maybeX in ...`.

`As` is for assigning a variable to a pattern: `let Just ({a, b} as rec) = maybeRec in ...`.

`String` is for strings: `let Just "x" = maybeX in ...`.

-}
type Pattern
    = Record
    | Tuple
    | Ignore
    | Named
    | As
    | String


{-| Type of search.

When searching for a list of functions, we can specify what constitutes a success.

Use `All` to make sure all functions are called, `None` to make sure no function is called, and `Some` to make sure at least one function is called.

-}
type Find
    = All
    | None
    | Some


type FoundExpression
    = NotFound CalledExpression
    | FoundAt Range


type Context
    = Context
        { functionDeclaration : Maybe FunctionName
        , calledFromRange : Maybe Range
        , foundExpressions : List FoundExpression
        , lookupTable : ModuleNameLookupTable
        , callTree : Dict FunctionName (List (Node Expression))
        , argumentTree : Dict FunctionName (List (Node Pattern.Pattern))
        }


{-| Check if a list of functions are called from a module or a top-level function.

`calledFrom` indicates if the functions should be searched through the whole module (`Anywhere`) or within a particular function (`TopFunction functionA`).

`findExpressions` specifies the list of functions or expressions that we are looking for, for example `functions = [FromExternalModule ["List", "Extra"] "find", FromExternalModule ["List", "Extra"] "findIndex"]`.

`find` specifies the type of search. For example, we might want to forbid the use of `List.Extra.find` and `List.Extra.findIndex`, or maybe we want to make sure that one of these is being called, or even both.

`comment` is the comment that will be returned if the module doesn't satisfy the specifications.

Let's consider an example:

    module A exposing (..)

    functionA param = param
        |> functionB
        |> List.Extra.find List.isEmpty

    functionB param =
        |> List.Extra.findIndex (\(i, _) -> i > 0)
        |> List.map Tuple.second

The rule `functionCalls {calledFrom = Anywhere, findExpressions = functions, find = All, comment = comment}` would succeed since `List.Extra.find` and `List.Extra.findIndex` are both called somewhere in the module. Changing to `calledFrom = TopFunction "functionB"` would fail, since `functionB` only calls one of the two, and `comment` would be thrown.

`functionCalls` can keep track of imports and aliases.
If module `A` was using `import List.Extra exposing (find, findIndex)` or `import List.Extra as List`, the rule would behave the same.

`functionCalls` searches recursively through internal top-level functions calls.
This is particularly useful when helper functions are defined.
For example, the rule above with `calledFrom = TopFunction "functionA"` would succeed, since `functionA` calls `List.Extra.find` directly, also calls `functionB` that itself calls `List.Extra.findIndex`, therefore we consider that `functionA` calls both functions.

-}
functionCalls :
    { calledFrom : CalledFrom
    , findExpressions : List CalledExpression
    , find : Find
    }
    -> Comment
    -> Rule
functionCalls { calledFrom, findExpressions, find } comment =
    Rule.newModuleRuleSchemaUsingContextCreator comment.path (initialContext findExpressions)
        |> Rule.withDeclarationEnterVisitor (annotateFunctionDeclaration calledFrom)
        |> Rule.withDeclarationEnterVisitor grabFunctionDeclarationArguments
        |> Rule.withExpressionEnterVisitor expressionCallsFunction
        |> Rule.withDeclarationExitVisitor annotateLeaveDeclaration
        |> Rule.withFinalModuleEvaluation (flattenTrees calledFrom >> checkForError find comment)
        |> Rule.fromModuleRuleSchema


initialContext : List CalledExpression -> Rule.ContextCreator () Context
initialContext functions =
    Rule.initContextCreator
        (\lookupTable () ->
            Context
                { lookupTable = lookupTable
                , functionDeclaration = Nothing
                , calledFromRange = Nothing
                , foundExpressions = List.map NotFound functions
                , callTree = Dict.empty
                , argumentTree = Dict.empty
                }
        )
        |> Rule.withModuleNameLookupTable


{-| Keep track of which top level function we are in
-}
annotateFunctionDeclaration : CalledFrom -> Node Declaration -> Context -> ( List nothing, Context )
annotateFunctionDeclaration calledFrom node (Context context) =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                (Node range functionName) =
                    Node.value declaration |> .name
            in
            ( []
            , Context
                { context
                    | functionDeclaration = Just functionName
                    , calledFromRange =
                        if calledFrom == TopFunction functionName then
                            Just range

                        else
                            context.calledFromRange

                    -- all function declaration must be represented in callTree to match the shape of argumentTree
                    , callTree = Dict.insert functionName [] context.callTree
                }
            )

        _ ->
            ( [], Context context )


{-| Gather the function arguments
-}
grabFunctionDeclarationArguments : Node Declaration -> Context -> ( List nothing, Context )
grabFunctionDeclarationArguments node (Context ({ argumentTree } as context)) =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                { name, arguments } =
                    Node.value declaration

                updateTree args =
                    Just (List.concatMap ElmSyntaxHelpers.traversePattern arguments ++ Maybe.withDefault [] args)
            in
            ( []
            , Context
                { context | argumentTree = Dict.update (Node.value name) updateTree argumentTree }
            )

        _ ->
            ( [], Context context )


{-| Gather all expressions we care about from the code in a tree
-}
expressionCallsFunction : Node Expression -> Context -> ( List nothing, Context )
expressionCallsFunction ((Node range expression) as node) (Context ({ lookupTable, functionDeclaration, callTree } as context)) =
    let
        add expr tree =
            Just (expr :: Maybe.withDefault [] tree)

        updateTree nodeTree =
            case expression of
                FunctionOrValue _ name ->
                    case LookupTable.moduleNameFor lookupTable node of
                        Nothing ->
                            nodeTree

                        Just originalModuleName ->
                            add (Node range (FunctionOrValue originalModuleName name)) nodeTree

                LetExpression _ ->
                    add node nodeTree

                CaseExpression _ ->
                    add node nodeTree

                RecordUpdateExpression _ _ ->
                    add node nodeTree

                OperatorApplication _ _ _ _ ->
                    add node nodeTree

                PrefixOperator _ ->
                    add node nodeTree

                LambdaExpression _ ->
                    add node nodeTree

                _ ->
                    nodeTree
    in
    ( []
    , Context
        { context
            | callTree =
                case functionDeclaration of
                    Just function ->
                        Dict.update function updateTree callTree

                    Nothing ->
                        callTree
        }
    )


annotateLeaveDeclaration : Node Declaration -> Context -> ( List nothing, Context )
annotateLeaveDeclaration node (Context context) =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            ( [], Context { context | functionDeclaration = Nothing } )

        _ ->
            ( [], Context context )


flattenTrees : CalledFrom -> Context -> Context
flattenTrees calledFrom (Context ({ callTree, argumentTree } as context)) =
    let
        ( allExpressions, allArguments ) =
            case calledFrom of
                Anywhere ->
                    ( Dict.values callTree |> List.concat
                    , Dict.values argumentTree |> List.concat
                    )

                TopFunction topFunction ->
                    traverseTreesFrom topFunction callTree argumentTree

        expressionsFound =
            context.foundExpressions
                |> List.map (\expression -> List.foldl matchExpression expression allExpressions)
                |> List.map (\expression -> List.foldl matchArgumentPattern expression allArguments)
    in
    Context { context | foundExpressions = expressionsFound }


traverseTreesFrom :
    FunctionName
    -> Dict FunctionName (List (Node Expression))
    -> Dict FunctionName (List (Node Pattern.Pattern))
    -> ( List (Node Expression), List (Node Pattern.Pattern) )
traverseTreesFrom root callTree argumentTree =
    case ( Dict.get root callTree, Dict.get root argumentTree ) of
        ( Just expressions, Just arguments ) ->
            let
                trimmedCallTree =
                    Dict.remove root callTree

                trimmedArgumentTree =
                    Dict.remove root argumentTree

                keepModuleFunction (Node _ expression) =
                    case expression of
                        FunctionOrValue [] name ->
                            Just name

                        _ ->
                            Nothing

                results =
                    expressions
                        |> List.filterMap keepModuleFunction
                        |> List.map (\branch -> traverseTreesFrom branch trimmedCallTree trimmedArgumentTree)
            in
            ( expressions ++ List.concatMap Tuple.first results
            , arguments ++ List.concatMap Tuple.second results
            )

        ( Nothing, Nothing ) ->
            ( [], [] )

        -- this should not happen, all function declarations have corresponding arguments
        _ ->
            ( [], [] )


matchExpression : Node Expression -> FoundExpression -> FoundExpression
matchExpression (Node range expression) foundExpression =
    let
        match a b =
            if a == b then
                foundAt range foundExpression

            else
                foundExpression

        letDestructuringPattern declaration =
            case Node.value declaration of
                LetDestructuring pattern _ ->
                    Just pattern

                LetFunction _ ->
                    Nothing
    in
    case ( expression, foundExpression ) of
        ( FunctionOrValue exprModule _, NotFound (AnyFromExternalModule extModule) ) ->
            match exprModule extModule

        ( FunctionOrValue exprModule exprName, NotFound (FromExternalModule extModule extName) ) ->
            match ( exprModule, exprName ) ( extModule, extName )

        ( FunctionOrValue [] exprName, NotFound (FromSameModule name) ) ->
            match exprName name

        ( LetExpression _, NotFound LetBlock ) ->
            FoundAt range

        ( CaseExpression _, NotFound CaseBlock ) ->
            FoundAt range

        ( RecordUpdateExpression _ _, NotFound RecordUpdate ) ->
            FoundAt range

        ( OperatorApplication exprOperator _ _ _, NotFound (Operator operator) ) ->
            match exprOperator operator

        ( PrefixOperator exprOperator, NotFound (Operator operator) ) ->
            match exprOperator operator

        ( LetExpression { declarations }, NotFound (LetWithPattern _) ) ->
            declarations
                |> List.filterMap letDestructuringPattern
                |> List.concatMap ElmSyntaxHelpers.traversePattern
                |> List.foldl matchExpressionPattern foundExpression

        ( CaseExpression { cases }, NotFound (CaseWithPattern _) ) ->
            cases
                |> List.map Tuple.first
                |> List.concatMap ElmSyntaxHelpers.traversePattern
                |> List.foldl matchExpressionPattern foundExpression

        ( LambdaExpression { args }, NotFound (LambdaWithPattern _) ) ->
            args
                |> List.concatMap ElmSyntaxHelpers.traversePattern
                |> List.foldl matchExpressionPattern foundExpression

        -- already found expression, argument pattern, or expression that doesn't match
        _ ->
            foundExpression


matchArgumentPattern : Node Pattern.Pattern -> FoundExpression -> FoundExpression
matchArgumentPattern node foundExpression =
    case foundExpression of
        NotFound (ArgumentWithPattern pattern) ->
            matchPattern node pattern foundExpression

        _ ->
            foundExpression


matchExpressionPattern : Node Pattern.Pattern -> FoundExpression -> FoundExpression
matchExpressionPattern node foundExpression =
    case foundExpression of
        NotFound (LetWithPattern pattern) ->
            matchPattern node pattern foundExpression

        NotFound (CaseWithPattern pattern) ->
            matchPattern node pattern foundExpression

        NotFound (LambdaWithPattern pattern) ->
            matchPattern node pattern foundExpression

        _ ->
            foundExpression


matchPattern : Node Pattern.Pattern -> Pattern -> FoundExpression -> FoundExpression
matchPattern (Node range elmSyntaxPattern) pattern =
    case ( elmSyntaxPattern, pattern ) of
        ( Pattern.RecordPattern _, Record ) ->
            foundAt range

        ( Pattern.TuplePattern _, Tuple ) ->
            foundAt range

        ( Pattern.AllPattern, Ignore ) ->
            foundAt range

        ( Pattern.NamedPattern _ _, Named ) ->
            foundAt range

        ( Pattern.AsPattern _ _, As ) ->
            foundAt range

        ( Pattern.StringPattern _, String ) ->
            foundAt range

        _ ->
            identity


foundAt : Range -> FoundExpression -> FoundExpression
foundAt range foundFunction =
    case foundFunction of
        NotFound _ ->
            FoundAt range

        FoundAt _ ->
            foundFunction


checkForError : Find -> Comment -> Context -> List (Error {})
checkForError find comment (Context { foundExpressions, calledFromRange }) =
    --  errors do not actually export a range at this point, but they could
    case find of
        All ->
            -- looking at functions not found
            case ( List.filter (functionWasFound >> not) foundExpressions, calledFromRange ) of
                -- all were found, no error
                ( [], _ ) ->
                    []

                -- some were not found, searching with TopFunction, return error with the range of the TopFunction
                ( _ :: _, Just range ) ->
                    -- Head is the function not found, could be included in the message / parameters
                    [ Comment.createError comment range ]

                -- some were not found, searching with Anywhere, return global error since no range exists
                ( _ :: _, Nothing ) ->
                    -- Head is the function not found, could be included in the message / parameters
                    [ Comment.createGlobalError comment ]

        None ->
            -- looking at found functions
            case List.filter functionWasFound foundExpressions of
                -- some were found, return error with range of the first one
                (FoundAt range) :: _ ->
                    -- Head is the function not found, could be included in the message / parameters
                    [ Comment.createError comment range ]

                -- none were found, no error
                _ ->
                    []

        Some ->
            -- looking at found functions
            case ( List.filter functionWasFound foundExpressions, calledFromRange ) of
                -- none were found, searching with TopFunction, return error with the range of the TopFunction
                ( [], Just range ) ->
                    [ Comment.createError comment range ]

                -- none were found, searching with Anywhere, return global error since no range exists
                ( [], Nothing ) ->
                    [ Comment.createGlobalError comment ]

                -- some were found, no error
                _ ->
                    []


functionWasFound : FoundExpression -> Bool
functionWasFound foundFunction =
    case foundFunction of
        NotFound _ ->
            False

        FoundAt _ ->
            True
