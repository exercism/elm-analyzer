module ElmSyntaxHelpers exposing (hasGenericRecord, typeAnnotationsMatch)

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))


typeAnnotationsMatch : Node TypeAnnotation -> Node TypeAnnotation -> Bool
typeAnnotationsMatch a b =
    let
        listsMatchWith f x y =
            (List.length x == List.length y) && (List.map2 f x y |> List.all identity)
    in
    case ( Node.value a, Node.value b ) of
        ( GenericType aName, GenericType bName ) ->
            aName == bName

        ( Typed aName aArgs, Typed bName bArgs ) ->
            (Node.value aName == Node.value bName)
                && listsMatchWith typeAnnotationsMatch aArgs bArgs

        ( Unit, Unit ) ->
            True

        ( Tupled aArgs, Tupled bArgs ) ->
            listsMatchWith typeAnnotationsMatch aArgs bArgs

        ( Record aRecordFields, Record bRecordFields ) ->
            listsMatchWith
                (\(Node _ ( aName, aType )) (Node _ ( bName, bType )) ->
                    (Node.value aName == Node.value bName)
                        && typeAnnotationsMatch aType bType
                )
                aRecordFields
                bRecordFields

        ( GenericRecord aName (Node _ aRecordFields), GenericRecord bName (Node _ bRecordFields) ) ->
            (Node.value aName == Node.value bName)
                && listsMatchWith
                    (\(Node _ ( aInnerName, aType )) (Node _ ( bInnerName, bType )) ->
                        (Node.value aInnerName == Node.value bInnerName)
                            && typeAnnotationsMatch aType bType
                    )
                    aRecordFields
                    bRecordFields

        ( FunctionTypeAnnotation a1 a2, FunctionTypeAnnotation b1 b2 ) ->
            typeAnnotationsMatch a1 b1 && typeAnnotationsMatch a2 b2

        _ ->
            False


hasGenericRecord : Node TypeAnnotation -> Bool
hasGenericRecord annotation =
    case Node.value annotation of
        GenericRecord _ _ ->
            True

        Typed _ annotations ->
            List.any hasGenericRecord annotations

        Tupled annotations ->
            List.any hasGenericRecord annotations

        FunctionTypeAnnotation a b ->
            hasGenericRecord a || hasGenericRecord b

        Record _ ->
            False

        GenericType _ ->
            False

        Unit ->
            False