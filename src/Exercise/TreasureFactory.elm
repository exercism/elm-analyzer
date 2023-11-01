module Exercise.TreasureFactory exposing (makeChestSignatureMatchesStub, makeTreasureChestSignatureMatchesStub, ruleConfig, secureChestSignatureIsCorrect, uniqueTreasuresSignatureIsCorrect)

import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import ElmSyntaxHelpers
import Review.Rule as Rule exposing (Error, Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "treasure-factory"
    , restrictToFiles = Just [ "src/TreasureFactory.elm" ]
    , rules =
        [ CustomRule makeChestSignatureMatchesStub
            (Comment "makeChest signature was changed" "elm.treasure-factory.do_not_change_given_signatures" Essential Dict.empty)
        , CustomRule makeTreasureChestSignatureMatchesStub
            (Comment "makeTreasureChest signature was changed" "elm.treasure-factory.do_not_change_given_signatures" Essential Dict.empty)
        , CustomRule secureChestSignatureIsCorrect
            (Comment "secureChest signature is incorrect" "elm.treasure-factory.incorrect_secureChest_signature" Essential Dict.empty)
        , CustomRule uniqueTreasuresSignatureIsCorrect
            (Comment "uniqueTreasures signature is incorrect" "elm.treasure-factory.incorrect_uniqueTreasures_signature" Essential Dict.empty)
        ]
    }


makeChestSignatureMatchesStub : Comment -> Rule
makeChestSignatureMatchesStub comment =
    let
        expectedSignature =
            { name = Node.empty "makeChest"
            , typeAnnotation =
                Node.empty
                    (FunctionTypeAnnotation
                        (Node.empty (Typed (Node.empty ( [], "String" )) []))
                        (Node.empty
                            (FunctionTypeAnnotation
                                (Node.empty (GenericType "treasure"))
                                (Node.empty
                                    (Typed
                                        (Node.empty ( [], "Chest" ))
                                        [ Node.empty (GenericType "treasure")
                                        , Node.empty (Record [])
                                        ]
                                    )
                                )
                            )
                        )
                    )
            }
    in
    signatureMustBe comment expectedSignature


makeTreasureChestSignatureMatchesStub : Comment -> Rule
makeTreasureChestSignatureMatchesStub comment =
    let
        expectedSignature =
            { name = Node.empty "makeTreasureChest"
            , typeAnnotation =
                Node.empty
                    (FunctionTypeAnnotation
                        (Node.empty
                            (Typed
                                (Node.empty ( [], "Chest" ))
                                [ Node.empty (GenericType "treasure")
                                , Node.empty
                                    (GenericRecord
                                        (Node.empty "conditions")
                                        (Node.empty
                                            [ Node.empty ( Node.empty "securePassword", Node.empty Unit )
                                            , Node.empty ( Node.empty "uniqueTreasure", Node.empty Unit )
                                            ]
                                        )
                                    )
                                ]
                            )
                        )
                        (Node.empty
                            (Typed
                                (Node.empty ( [], "TreasureChest" ))
                                [ Node.empty (GenericType "treasure") ]
                            )
                        )
                    )
            }
    in
    signatureMustBe comment expectedSignature


secureChestSignatureIsCorrect : Comment -> Rule
secureChestSignatureIsCorrect comment =
    let
        expectedSignature =
            { name = Node.empty "secureChest"
            , typeAnnotation =
                Node.empty
                    (FunctionTypeAnnotation
                        (Node.empty
                            (Typed
                                (Node.empty ( [], "Chest" ))
                                [ Node.empty (GenericType "treasure")
                                , Node.empty (GenericType "conditions")
                                ]
                            )
                        )
                        (Node.empty
                            (Typed
                                (Node.empty ( [], "Maybe" ))
                                [ Node.empty
                                    (Typed
                                        (Node.empty ( [], "Chest" ))
                                        [ Node.empty (GenericType "treasure")
                                        , Node.empty
                                            (GenericRecord
                                                (Node.empty "conditions")
                                                (Node.empty
                                                    [ Node.empty ( Node.empty "securePassword", Node.empty Unit ) ]
                                                )
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                    )
            }
    in
    signatureMustBe comment expectedSignature


uniqueTreasuresSignatureIsCorrect : Comment -> Rule
uniqueTreasuresSignatureIsCorrect comment =
    let
        expectedSignature =
            { name = Node.empty "uniqueTreasures"
            , typeAnnotation =
                Node.empty
                    (FunctionTypeAnnotation
                        (Node.empty
                            (Typed
                                (Node.empty ( [], "List" ))
                                [ Node.empty
                                    (Typed
                                        (Node.empty ( [], "Chest" ))
                                        [ Node.empty (GenericType "treasure")
                                        , Node.empty (GenericType "conditions")
                                        ]
                                    )
                                ]
                            )
                        )
                        (Node.empty
                            (Typed
                                (Node.empty ( [], "List" ))
                                [ Node.empty
                                    (Typed
                                        (Node.empty ( [], "Chest" ))
                                        [ Node.empty (GenericType "treasure")
                                        , Node.empty
                                            (GenericRecord
                                                (Node.empty "conditions")
                                                (Node.empty
                                                    [ Node.empty
                                                        ( Node.empty "uniqueTreasure"
                                                        , Node.empty Unit
                                                        )
                                                    ]
                                                )
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
                    )
            }
    in
    signatureMustBe comment expectedSignature


signatureMustBe : Comment -> Signature -> Rule
signatureMustBe comment expectedSignature =
    Rule.newModuleRuleSchema comment.path []
        |> Rule.withSimpleDeclarationVisitor (hasExpectedSignatureVisitor comment expectedSignature)
        |> Rule.fromModuleRuleSchema


hasExpectedSignatureVisitor : Comment -> Signature -> Node Declaration -> List (Error {})
hasExpectedSignatureVisitor comment expectedSignature node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration, signature } ->
            if (declaration |> Node.value |> .name |> Node.value) == Node.value expectedSignature.name then
                let
                    error =
                        [ Comment.createError comment (declaration |> Node.value |> .name |> Node.range) ]
                in
                case signature of
                    Nothing ->
                        error

                    Just (Node _ { typeAnnotation }) ->
                        if ElmSyntaxHelpers.typeAnnotationsMatch typeAnnotation expectedSignature.typeAnnotation then
                            []

                        else
                            error

            else
                []

        _ ->
            []
