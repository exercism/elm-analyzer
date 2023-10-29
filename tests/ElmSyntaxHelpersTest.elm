module ElmSyntaxHelpersTest exposing (..)

import Elm.Syntax.Node as Node exposing (Node(..))
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
