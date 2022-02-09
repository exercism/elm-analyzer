module Analyzer exposing (CalledFunction(..), Find(..), functionCalls)

import Comment exposing (Comment, CommentType(..))
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as LookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Functions that can be searched for.

`AnyFromExternalModule ["List", "Extra"]` means that we are looking for any function from the
`List.Extra` module.

`FromExternalModule ["List", "Extra"] "find"` means that we a looking for a call to
`List.Extra.find`.

Note that for searching for function that are automatically imported, like `round`, the module
must be explicitely stated: `FromExternalModule ["Basics"] "round"`.

`FromSameModule "solve"` means that we are looking for a call to a top-level function defined
in the same module.

-}



-- TODO: also search for operators, let, case, if, or any expression?


type CalledFunction
    = AnyFromExternalModule ModuleName
    | FromExternalModule ModuleName String
    | FromSameModule String


{-| Type of search.

When searching for a list of functions, we can specify what constitutes a success.

Use `All` to make sure all functions are called, `None` to make sure no function is called,
and `Some` to make sure at least one function is called.

-}
type Find
    = All
    | None
    | Some


type Context
    = Context
        { functionDeclaration : Maybe String
        , calledFromRange : Maybe Range
        , functionsFound : List ( CalledFunction, Maybe Range )
        , lookupTable : ModuleNameLookupTable
        , callTree : Dict String (List ( Maybe ModuleName, String, Range ))
        }


{-| Check if a list of functions are called from a module or a top-level function.

`calledFrom` indicates if the functions should be searched through the whole module (`Nothing`)
or within a particular function (`Just functionA`).

`findFunctions` specifies the list of functions that we are looking for, for example
`functions = [FromExternalModule ["List", "Extra"] "find", FromExternalModule ["List", "Extra"] "findIndex"]`.

`find` specifies the type of search. For example, we might want to forbid the use of
`List.Extra.find` and `List.Extra.findIndex`, or maybe we want to make sure that one of these
is being called, or even both.

`comment` is the comment that will be returned if the module doesn't satisfy the specifications.

Let's consider an example:

    module A exposing (..)

    functionA param = param
        |> functionB
        |> List.Extra.find List.isEmpty

    functionB param =
        |> List.Extra.findIndex (\(i, _) -> i > 0)
        |> List.map Tuple.second

The rule `functionCalls {calledFrom = Nothing, findFunctions = functions, find = All, comment = comment}`
would succeed since `List.Extra.find` and `List.Extra.findIndex` are both called somewhere in the
module. Changing to `calledFrom = Just "functionB"` would fail, since `functionB` only calls one of
the two, and `comment` would be thrown.

`functionCalls` can keep track of imports and aliases. If module `A` was using
`import List.Extra exposing (find, findIndex)` or `import List.Extra as List`, the rule would
behave the same.

`functionCalls` searches recursively through internal top-level functions calls.
This is particularly useful when helper functions are defined. For example, the rule above with
`calledFrom = Just "functionA"` would succeed, since `functionA` calls `List.Extra.find` directly,
also calls `functionB` that itself calls `List.Extra.findIndex`, therefore we consider that
`functionA` calls both functions.

-}
functionCalls :
    { calledFrom : Maybe String
    , findFunctions : List CalledFunction
    , find : Find
    , comment : Comment
    }
    -> Rule
functionCalls { calledFrom, findFunctions, find, comment } =
    Rule.newModuleRuleSchemaUsingContextCreator comment.comment (initialContext findFunctions)
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
                , functionsFound = List.map (\function -> ( function, Nothing )) functions
                , callTree = Dict.empty
                }
        )
        |> Rule.withModuleNameLookupTable


annotateFunctionDeclaration : Maybe String -> Node Declaration -> Context -> ( List (Error {}), Context )
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
                        if calledFrom == Just functionName then
                            Just range

                        else
                            context.calledFromRange
                }
            )

        _ ->
            ( [], Context context )


expressionCallsFunction : Node Expression -> Context -> ( List (Error {}), Context )
expressionCallsFunction expression (Context ({ lookupTable, functionDeclaration, callTree } as context)) =
    case Node.value expression of
        Expression.FunctionOrValue _ name ->
            let
                moduleName =
                    LookupTable.moduleNameFor lookupTable expression

                addfunctionCall =
                    Maybe.withDefault [] >> (::) ( moduleName, name, Node.range expression ) >> Just
            in
            ( []
            , Context
                { context
                    | callTree =
                        case functionDeclaration of
                            Just function ->
                                Dict.update function addfunctionCall callTree

                            Nothing ->
                                callTree
                }
            )

        _ ->
            ( [], Context context )


annotateLeaveDeclaration : Node Declaration -> Context -> ( List (Error {}), Context )
annotateLeaveDeclaration node (Context context) =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            ( [], Context { context | functionDeclaration = Nothing } )

        _ ->
            ( [], Context context )


flattenTree : Maybe String -> Context -> Context
flattenTree calledFrom (Context ({ callTree } as context)) =
    let
        allExpressions =
            case calledFrom of
                Nothing ->
                    Dict.values callTree |> List.concat

                Just topFunction ->
                    traverseTreeFrom topFunction callTree

        functionsFound =
            List.map (\function -> List.foldl matchFunction function allExpressions) context.functionsFound
    in
    Context { context | functionsFound = functionsFound }


traverseTreeFrom :
    String
    -> Dict String (List ( Maybe ModuleName, String, Range ))
    -> List ( Maybe ModuleName, String, Range )
traverseTreeFrom root tree =
    case Dict.get root tree of
        Nothing ->
            []

        Just expressions ->
            let
                keepModuleFunction ( moduleName, function, _ ) =
                    if moduleName == Just [] then
                        Just function

                    else
                        Nothing

                modulefunctions =
                    List.filterMap keepModuleFunction expressions

                trimmedTree =
                    Dict.remove root tree
            in
            expressions ++ List.concatMap (\branch -> traverseTreeFrom branch trimmedTree) modulefunctions


matchFunction :
    ( Maybe ModuleName, String, Range )
    -> ( CalledFunction, Maybe Range )
    -> ( CalledFunction, Maybe Range )
matchFunction ( functionModule, functionName, range ) ( calledFunction, found ) =
    let
        getRange match =
            if match then
                Just range

            else
                Nothing
    in
    case ( calledFunction, found ) of
        ( _, Just _ ) ->
            ( calledFunction, found )

        ( AnyFromExternalModule externalModule, Nothing ) ->
            ( calledFunction, functionModule == Just externalModule |> getRange )

        ( FromExternalModule externalModule name, Nothing ) ->
            ( calledFunction, ( Just externalModule, name ) == ( functionModule, functionName ) |> getRange )

        ( FromSameModule name, Nothing ) ->
            ( calledFunction, ( Just [], name ) == ( functionModule, functionName ) |> getRange )


checkForError : Find -> Comment -> Context -> List (Error {})
checkForError find comment (Context { functionsFound, calledFromRange }) =
    case find of
        All ->
            case ( List.filter (Tuple.second >> isNothing) functionsFound, calledFromRange ) of
                ( [], _ ) ->
                    []

                ( _ :: _, Just range ) ->
                    -- Head is the function not found, could be included in the message / parameters
                    [ Comment.createError comment range ]

                ( _ :: _, Nothing ) ->
                    -- Head is the function not found, could be included in the message / parameters
                    [ Comment.createGlobalError comment ]

        None ->
            case List.filter (Tuple.second >> isNothing >> not) functionsFound of
                ( _, Just range ) :: _ ->
                    -- Head is the function not found, could be included in the message / parameters
                    [ Comment.createError comment range ]

                _ ->
                    []

        Some ->
            case ( List.filter (Tuple.second >> isNothing >> not) functionsFound, calledFromRange ) of
                ( [], Just range ) ->
                    [ Comment.createError comment range ]

                ( [], Nothing ) ->
                    [ Comment.createGlobalError comment ]

                _ ->
                    []


isNothing : Maybe a -> Bool
isNothing =
    (==) Nothing
