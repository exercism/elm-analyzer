module ElmSyntaxHelpersTest exposing (..)

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import ElmSyntaxHelpers
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test)


typeAnnotationsMatchTests : Test
typeAnnotationsMatchTests =
    describe "typeAnnotationsMatch"
        [ test "ranges are ignored" <|
            \_ ->
                let
                    noRangeUnit =
                        Node.empty Unit

                    someRangeUnit =
                        Node { start = { row = 5, column = 1 }, end = { row = 5, column = 16 } } Unit
                in
                ElmSyntaxHelpers.typeAnnotationsMatch noRangeUnit someRangeUnit
                    |> Expect.equal True
        , fuzz typeAnnotationsFuzzer "a TypeAnnotation matches itself" <|
            \a ->
                ElmSyntaxHelpers.typeAnnotationsMatch a a
                    |> Expect.equal True
        , fuzz2 typeAnnotationsFuzzer typeAnnotationsFuzzer "only matches equal TypeAnnotations" <|
            \a b ->
                ElmSyntaxHelpers.typeAnnotationsMatch a b
                    |> Expect.equal (a == b)
        ]


hasGenericRecordTests : Test
hasGenericRecordTests =
    describe "hasGenericRecord"
        [ test "no GenericRecord" <|
            \_ ->
                Node.empty (Tupled [ Node.empty Unit ])
                    |> ElmSyntaxHelpers.hasGenericRecord
                    |> Expect.equal False
        , test "one GenericRecord" <|
            \_ ->
                Node.empty (Tupled [ Node.empty Unit, Node.empty (GenericRecord (Node.empty "a") (Node.empty [])) ])
                    |> ElmSyntaxHelpers.hasGenericRecord
                    |> Expect.equal True
        ]


traversePatternTests : Test
traversePatternTests =
    describe "traversePattern"
        [ test "non recursive patterns are not modified" <|
            \_ ->
                let
                    patterns =
                        [ AllPattern
                        , UnitPattern
                        , CharPattern 'a'
                        , StringPattern "a"
                        , IntPattern 0
                        , HexPattern 0x00
                        , FloatPattern 3.14
                        , VarPattern "x"
                        , RecordPattern [ Node.empty "a", Node.empty "b" ]
                        ]
                            |> List.map Node.empty
                in
                patterns
                    |> List.concatMap ElmSyntaxHelpers.traversePattern
                    |> Expect.equal patterns
        , test "patterns with one pattern argument are placed before to their traversed argument" <|
            \_ ->
                let
                    patterns =
                        [ AsPattern (Node.empty UnitPattern) (Node.empty "y")
                        , ParenthesizedPattern (Node.empty AllPattern)
                        ]
                            |> List.map Node.empty
                in
                patterns
                    |> List.concatMap ElmSyntaxHelpers.traversePattern
                    |> Expect.equal
                        ([ AsPattern (Node.empty UnitPattern) (Node.empty "y")
                         , UnitPattern
                         , ParenthesizedPattern (Node.empty AllPattern)
                         , AllPattern
                         ]
                            |> List.map Node.empty
                        )
        , test "patterns with two pattern arguments are placed before their traversed arguments" <|
            \_ ->
                let
                    pattern =
                        Node.empty (UnConsPattern (Node.empty (VarPattern "x")) (Node.empty (VarPattern "xs")))
                in
                pattern
                    |> ElmSyntaxHelpers.traversePattern
                    |> Expect.equal
                        ([ UnConsPattern (Node.empty (VarPattern "x")) (Node.empty (VarPattern "xs"))
                         , VarPattern "x"
                         , VarPattern "xs"
                         ]
                            |> List.map Node.empty
                        )
        , test "patterns with a list of pattern arguments are placed before their traversed arguments" <|
            \_ ->
                let
                    patterns =
                        [ TuplePattern [ Node.empty (VarPattern "x"), Node.empty (VarPattern "y") ]
                        , ListPattern [ Node.empty (CharPattern 'a'), Node.empty (CharPattern 'b') ]
                        , NamedPattern { moduleName = [], name = "Maybe" } [ Node.empty (IntPattern 0), Node.empty (HexPattern 0x00) ]
                        ]
                            |> List.map Node.empty
                in
                patterns
                    |> List.concatMap ElmSyntaxHelpers.traversePattern
                    |> Expect.equal
                        ([ TuplePattern [ Node.empty (VarPattern "x"), Node.empty (VarPattern "y") ]
                         , VarPattern "x"
                         , VarPattern "y"
                         , ListPattern [ Node.empty (CharPattern 'a'), Node.empty (CharPattern 'b') ]
                         , CharPattern 'a'
                         , CharPattern 'b'
                         , NamedPattern { moduleName = [], name = "Maybe" } [ Node.empty (IntPattern 0), Node.empty (HexPattern 0x00) ]
                         , IntPattern 0
                         , HexPattern 0x00
                         ]
                            |> List.map Node.empty
                        )
        , test "deeply nested patterns work" <|
            \_ ->
                let
                    pattern =
                        TuplePattern
                            [ Node.empty (VarPattern "x")
                            , ListPattern
                                [ Node.empty (CharPattern 'a')
                                , NamedPattern { moduleName = [], name = "Maybe" }
                                    [ Node.empty (IntPattern 0) ]
                                    |> Node.empty
                                ]
                                |> Node.empty
                            ]
                            |> Node.empty
                in
                pattern
                    |> ElmSyntaxHelpers.traversePattern
                    |> Expect.equal
                        ([ Node.value pattern
                         , VarPattern "x"
                         , ListPattern
                            [ Node.empty (CharPattern 'a')
                            , NamedPattern { moduleName = [], name = "Maybe" }
                                [ Node.empty (IntPattern 0) ]
                                |> Node.empty
                            ]
                         , CharPattern 'a'
                         , NamedPattern { moduleName = [], name = "Maybe" } [ Node.empty (IntPattern 0) ]
                         , IntPattern 0
                         ]
                            |> List.map Node.empty
                        )
        ]



-- FUZZERS


typeAnnotationsFuzzer : Fuzzer (Node TypeAnnotation)
typeAnnotationsFuzzer =
    let
        unit =
            Fuzz.constant Unit

        genericType =
            Fuzz.map GenericType Fuzz.string

        moduleType =
            Fuzz.pair (Fuzz.listOfLengthBetween 0 2 Fuzz.string) Fuzz.string
                |> Fuzz.map Node.empty

        annotation =
            Fuzz.lazy (\_ -> typeAnnotationsFuzzer)

        list =
            Fuzz.listOfLengthBetween 0 2 annotation

        typed =
            Fuzz.map2 Typed moduleType list

        tupled =
            Fuzz.map Tupled list

        string =
            Fuzz.map Node.empty Fuzz.string

        recordDefinition =
            Fuzz.pair string annotation
                |> Fuzz.map Node.empty
                |> Fuzz.listOfLengthBetween 0 2

        record =
            Fuzz.map Record recordDefinition

        genericRecord =
            Fuzz.map2 GenericRecord string (Fuzz.map Node.empty recordDefinition)

        functionTypeAnnotation =
            Fuzz.map2 FunctionTypeAnnotation annotation annotation
    in
    [ unit, genericType, typed, tupled, record, genericRecord, functionTypeAnnotation ]
        |> List.map (Fuzz.map Node.empty)
        |> Fuzz.oneOf
