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


type CalledFunction
    = AnyFromExternalModule ModuleName
    | FromExternalModule ModuleName String
    | FromSameModule String


type Find
    = All
    | None
    | Some


type alias Context =
    { functionDeclaration : Maybe String
    , calledFromRange : Maybe Range
    , functionsFound : List ( CalledFunction, Maybe Range )
    , lookupTable : ModuleNameLookupTable
    , callTree : Dict String (List ( Maybe ModuleName, String, Range ))
    }



{-


   TODO:
   - add documentation
     - Basic functions must be explicit
     - indirect calls
   - recognize operators, let, case, if...


-}


functionCalls :
    { calledFrom : Maybe String
    , findFunctions : List CalledFunction
    , find : Find
    , comment : Comment
    }
    -> Rule
functionCalls { calledFrom, findFunctions, find, comment } =
    Rule.newModuleRuleSchemaUsingContextCreator comment.name (initialContext findFunctions)
        |> Rule.withDeclarationEnterVisitor (annotateFunctionDeclaration calledFrom)
        |> Rule.withExpressionEnterVisitor expressionCallsFunction
        |> Rule.withDeclarationExitVisitor annotateLeaveDeclaration
        |> Rule.withFinalModuleEvaluation (flattenTree calledFrom >> checkForError find comment)
        |> Rule.fromModuleRuleSchema


initialContext : List CalledFunction -> Rule.ContextCreator () Context
initialContext functions =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , functionDeclaration = Nothing
            , calledFromRange = Nothing
            , functionsFound = List.map (\function -> ( function, Nothing )) functions
            , callTree = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


annotateFunctionDeclaration : Maybe String -> Node Declaration -> Context -> ( List (Error {}), Context )
annotateFunctionDeclaration calledFrom node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                (Node range functionName) =
                    Node.value declaration |> .name
            in
            ( []
            , { context
                | functionDeclaration = Just functionName
                , calledFromRange =
                    if calledFrom == Just functionName then
                        Just range

                    else
                        context.calledFromRange
              }
            )

        _ ->
            ( [], context )


expressionCallsFunction : Node Expression -> Context -> ( List (Error {}), Context )
expressionCallsFunction expression ({ lookupTable, functionDeclaration, callTree } as context) =
    case Node.value expression of
        Expression.FunctionOrValue _ name ->
            let
                moduleName =
                    LookupTable.moduleNameFor lookupTable expression

                addfunctionCall =
                    Maybe.withDefault [] >> (::) ( moduleName, name, Node.range expression ) >> Just
            in
            ( []
            , { context
                | callTree =
                    case functionDeclaration of
                        Just function ->
                            Dict.update function addfunctionCall callTree

                        Nothing ->
                            callTree
              }
            )

        _ ->
            ( [], context )


annotateLeaveDeclaration : Node Declaration -> Context -> ( List (Error {}), Context )
annotateLeaveDeclaration node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            ( [], { context | functionDeclaration = Nothing } )

        _ ->
            ( [], context )


flattenTree : Maybe String -> Context -> Context
flattenTree calledFrom ({ callTree } as context) =
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
    { context | functionsFound = functionsFound }


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
checkForError find comment { functionsFound, calledFromRange } =
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
