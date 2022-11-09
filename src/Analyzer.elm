module Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..), functionCalls)

import Comment exposing (Comment)
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
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


{-| Functions that can be searched for.

`AnyFromExternalModule ["List", "Extra"]` means that we are looking for any function from the `List.Extra` module.

`FromExternalModule ["List", "Extra"] "find"` means that we a looking for a call to `List.Extra.find`.

Note that for searching for function that are automatically imported, like `round`, the module must be explicitely stated: `FromExternalModule ["Basics"] "round"`.

`FromSameModule "solve"` means that we are looking for a call to a top-level function defined in the same module.

`LetBlock` means looking for a `let` expression.

`CaseBlock` means looking for a `case` expression.

-}



-- TODO: also search for operators, if, or any expression?


type CalledFunction
    = AnyFromExternalModule ModuleName
    | FromExternalModule ModuleName FunctionName
    | FromSameModule FunctionName
    | LetBlock
    | CaseBlock


{-| Type of search.

When searching for a list of functions, we can specify what constitutes a success.

Use `All` to make sure all functions are called, `None` to make sure no function is called, and `Some` to make sure at least one function is called.

-}
type Find
    = All
    | None
    | Some


type FoundFunction
    = NotFound CalledFunction
    | FoundAt Range


type Context
    = Context
        { functionDeclaration : Maybe FunctionName
        , calledFromRange : Maybe Range
        , foundFunctions : List FoundFunction
        , lookupTable : ModuleNameLookupTable
        , callTree : Dict FunctionName (List (Node Expression))
        }


{-| Check if a list of functions are called from a module or a top-level function.

`calledFrom` indicates if the functions should be searched through the whole module (`Anywhere`) or within a particular function (`TopFunction functionA`).

`findFunctions` specifies the list of functions that we are looking for, for example `functions = [FromExternalModule ["List", "Extra"] "find", FromExternalModule ["List", "Extra"] "findIndex"]`.

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

The rule `functionCalls {calledFrom = Anywhere, findFunctions = functions, find = All, comment = comment}` would succeed since `List.Extra.find` and `List.Extra.findIndex` are both called somewhere in the module. Changing to `calledFrom = TopFunction "functionB"` would fail, since `functionB` only calls one of the two, and `comment` would be thrown.

`functionCalls` can keep track of imports and aliases.
If module `A` was using `import List.Extra exposing (find, findIndex)` or `import List.Extra as List`, the rule would behave the same.

`functionCalls` searches recursively through internal top-level functions calls.
This is particularly useful when helper functions are defined.
For example, the rule above with `calledFrom = TopFunction "functionA"` would succeed, since `functionA` calls `List.Extra.find` directly, also calls `functionB` that itself calls `List.Extra.findIndex`, therefore we consider that `functionA` calls both functions.

-}
functionCalls :
    { calledFrom : CalledFrom
    , findFunctions : List CalledFunction
    , find : Find
    }
    -> Comment
    -> Rule
functionCalls { calledFrom, findFunctions, find } comment =
    Rule.newModuleRuleSchemaUsingContextCreator comment.path (initialContext findFunctions)
        |> Rule.withDeclarationEnterVisitor (annotateFunctionDeclaration calledFrom)
        |> Rule.withExpressionEnterVisitor expressionCallsFunction
        |> Rule.withDeclarationExitVisitor annotateLeaveDeclaration
        |> Rule.withFinalModuleEvaluation (flattenTree calledFrom >> checkForError find comment)
        |> Rule.fromModuleRuleSchema


initialContext : List CalledFunction -> Rule.ContextCreator () Context
initialContext functions =
    Rule.initContextCreator
        (\lookupTable () ->
            Context
                { lookupTable = lookupTable
                , functionDeclaration = Nothing
                , calledFromRange = Nothing
                , foundFunctions = List.map NotFound functions
                , callTree = Dict.empty
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
                }
            )

        _ ->
            ( [], Context context )


{-| Gather all expressions we care about from the code in a tree
-}
expressionCallsFunction : Node Expression -> Context -> ( List nothing, Context )
expressionCallsFunction (Node range expression) (Context ({ lookupTable, functionDeclaration, callTree } as context)) =
    let
        updateFunction treeExpressions =
            case expression of
                FunctionOrValue _ name ->
                    case LookupTable.moduleNameFor lookupTable (Node range expression) of
                        Nothing ->
                            treeExpressions

                        Just originalModuleName ->
                            Node range (FunctionOrValue originalModuleName name)
                                :: Maybe.withDefault [] treeExpressions
                                |> Just

                LetExpression block ->
                    Node range (LetExpression block)
                        :: Maybe.withDefault [] treeExpressions
                        |> Just

                CaseExpression block ->
                    Node range (CaseExpression block)
                        :: Maybe.withDefault [] treeExpressions
                        |> Just

                _ ->
                    treeExpressions
    in
    ( []
    , Context
        { context
            | callTree =
                case functionDeclaration of
                    Just function ->
                        Dict.update function updateFunction callTree

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


flattenTree : CalledFrom -> Context -> Context
flattenTree calledFrom (Context ({ callTree } as context)) =
    let
        allExpressions =
            case calledFrom of
                Anywhere ->
                    Dict.values callTree |> List.concat

                TopFunction topFunction ->
                    traverseTreeFrom topFunction callTree

        functionsFound =
            List.map (\function -> List.foldl matchFunction function allExpressions) context.foundFunctions
    in
    Context { context | foundFunctions = functionsFound }


traverseTreeFrom : FunctionName -> Dict FunctionName (List (Node Expression)) -> List (Node Expression)
traverseTreeFrom root tree =
    case Dict.get root tree of
        Nothing ->
            []

        Just expressions ->
            let
                keepModuleFunction (Node _ expression) =
                    case expression of
                        FunctionOrValue [] name ->
                            Just name

                        _ ->
                            Nothing

                modulefunctions =
                    List.filterMap keepModuleFunction expressions

                trimmedTree =
                    Dict.remove root tree
            in
            expressions ++ List.concatMap (\branch -> traverseTreeFrom branch trimmedTree) modulefunctions


matchFunction : Node Expression -> FoundFunction -> FoundFunction
matchFunction (Node range expression) foundFunction =
    let
        match a b =
            if a == b then
                foundAt range foundFunction

            else
                foundFunction
    in
    case ( expression, foundFunction ) of
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

        -- already found function, or expression that doesn't match
        _ ->
            foundFunction


foundAt : Range -> FoundFunction -> FoundFunction
foundAt range foundFunction =
    case foundFunction of
        NotFound _ ->
            FoundAt range

        FoundAt _ ->
            foundFunction


checkForError : Find -> Comment -> Context -> List (Error {})
checkForError find comment (Context { foundFunctions, calledFromRange }) =
    --  errors do not actually export a range at this point, but they could
    case find of
        All ->
            -- looking at functions not found
            case ( List.filter (functionWasFound >> not) foundFunctions, calledFromRange ) of
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
            case List.filter functionWasFound foundFunctions of
                -- some were found, return error with range of the first one
                (FoundAt range) :: _ ->
                    -- Head is the function not found, could be included in the message / parameters
                    [ Comment.createError comment range ]

                -- none were found, no error
                _ ->
                    []

        Some ->
            -- looking at found functions
            case ( List.filter functionWasFound foundFunctions, calledFromRange ) of
                -- none were found, searching with TopFunction, return error with the range of the TopFunction
                ( [], Just range ) ->
                    [ Comment.createError comment range ]

                -- none were found, searching with Anywhere, return global error since no range exists
                ( [], Nothing ) ->
                    [ Comment.createGlobalError comment ]

                -- some were found, no error
                _ ->
                    []


functionWasFound : FoundFunction -> Bool
functionWasFound foundFunction =
    case foundFunction of
        NotFound _ ->
            False

        FoundAt _ ->
            True
