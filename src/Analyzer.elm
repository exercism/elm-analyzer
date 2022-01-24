module Analyzer exposing (CalledFunction(..), Find(..), functionCalls)

import Comment exposing (Comment, CommentType(..))
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
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
    { isFunctionDeclaration : Bool
    , calledFromRange : Maybe Range
    , functionsFound : List ( CalledFunction, Maybe Range )
    , lookupTable : ModuleNameLookupTable
    }



{-


   TODO:
   - add documentation
     - Basic functions must be explicit
   - recognize operators
   - indirect calls


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
        |> Rule.withFinalModuleEvaluation (checkForError find comment)
        |> Rule.fromModuleRuleSchema


initialContext : List CalledFunction -> Rule.ContextCreator () Context
initialContext functions =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , isFunctionDeclaration = False
            , calledFromRange = Nothing
            , functionsFound = List.map (\function -> ( function, Nothing )) functions
            }
        )
        |> Rule.withModuleNameLookupTable


annotateFunctionDeclaration : Maybe String -> Node Declaration -> Context -> ( List (Error {}), Context )
annotateFunctionDeclaration calledFrom node context =
    case ( Node.value node, calledFrom ) of
        ( Declaration.FunctionDeclaration _, Nothing ) ->
            ( [], { context | isFunctionDeclaration = True } )

        ( Declaration.FunctionDeclaration { declaration }, Just functionName ) ->
            if functionName == (Node.value declaration |> .name |> Node.value) then
                ( []
                , { context
                    | isFunctionDeclaration = True
                    , calledFromRange = Just (declaration |> Node.value |> .name |> Node.range)
                  }
                )

            else
                ( [], context )

        _ ->
            ( [], context )


expressionCallsFunction : Node Expression -> Context -> ( List (Error {}), Context )
expressionCallsFunction expression ({ lookupTable, isFunctionDeclaration, functionsFound } as context) =
    case Node.value expression of
        Expression.FunctionOrValue _ function ->
            if isFunctionDeclaration then
                let
                    functionModule =
                        LookupTable.moduleNameFor lookupTable expression
                in
                ( []
                , { context
                    | functionsFound =
                        List.map (matchFunction functionModule function (Node.range expression)) functionsFound
                  }
                )

            else
                ( [], context )

        _ ->
            ( [], context )


matchFunction :
    Maybe ModuleName
    -> String
    -> Range
    -> ( CalledFunction, Maybe Range )
    -> ( CalledFunction, Maybe Range )
matchFunction functionModule functionName range ( calledFunction, found ) =
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


annotateLeaveDeclaration : Node Declaration -> Context -> ( List (Error {}), Context )
annotateLeaveDeclaration node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            ( [], { context | isFunctionDeclaration = False } )

        _ ->
            ( [], context )


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
