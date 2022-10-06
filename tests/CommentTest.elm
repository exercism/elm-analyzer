module CommentTest exposing (aggregateCommentsTest, encoderDecoderTest, makeSummaryTest)

import Comment exposing (Comment, CommentType(..), Summary)
import Common.NoUnused as NoUnused
import Common.Simplify as Simplify
import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import RuleConfig
import Test exposing (Test, describe, test)


fuzzComment : Fuzzer Comment
fuzzComment =
    Fuzz.map4 Comment
        Fuzz.string
        Fuzz.string
        fuzzCommentType
        (fuzzDict Fuzz.string Fuzz.string)


fuzzCommentType : Fuzzer CommentType
fuzzCommentType =
    Fuzz.oneOf
        [ Fuzz.constant Celebratory
        , Fuzz.constant Essential
        , Fuzz.constant Actionable
        , Fuzz.constant Informative
        ]


fuzzDict : Fuzzer comparable -> Fuzzer b -> Fuzzer (Dict comparable b)
fuzzDict key value =
    Fuzz.tuple ( key, value )
        |> Fuzz.list
        |> Fuzz.map Dict.fromList


encoderDecoderTest : Test
encoderDecoderTest =
    describe "encoders should encode and decoders should decode"
        [ test "encoding" <|
            \() ->
                Expect.equal
                    "{\"name\":\"name\",\"comment\":\"comment\",\"type\":\"celebratory\",\"params\":{\"key\":\"value\"}}"
                    (Comment "name" "comment" Celebratory (Dict.singleton "key" "value")
                        |> Comment.encodeComment
                        |> Encode.encode 0
                    )
        , test "decoding" <|
            \() ->
                Expect.equal
                    (Ok (Comment "name" "comment" Celebratory (Dict.singleton "key" "value")))
                    ("{\"name\":\"name\",\"comment\":\"comment\",\"type\":\"celebratory\",\"params\":{\"key\":\"value\"}}"
                        |> Decode.decodeString Comment.decodeComment
                    )
        , Test.fuzz fuzzComment "encodeComment then decodeComment should be identity" <|
            \comment ->
                Expect.equal (Ok comment)
                    (comment
                        |> Comment.encodeComment
                        |> Decode.decodeValue Comment.decodeComment
                    )
        ]


aggregateCommentsTest : Test
aggregateCommentsTest =
    let
        essential =
            Comment "" "" Essential Dict.empty

        actionable =
            Comment "" "" Actionable Dict.empty

        informative =
            Comment "" "" Informative Dict.empty

        celebratory =
            Comment "" "" Celebratory Dict.empty
    in
    describe "aggregateComments should order the comments and create message appropriately"
        [ test "no comments" <|
            \() -> Expect.equal (Comment.aggregateComments []) (Summary "No suggestions found." [])
        , test "if any comment is essential, summary is essential" <|
            \() ->
                Expect.equal
                    (Comment.aggregateComments [ informative, celebratory, essential, actionable ])
                    (Summary "Check the comments for things to fix.\u{202F}🛠 "
                        [ celebratory, essential, actionable, informative, Comment.feedbackComment ]
                    )
        , test "no essential, summary is actionable" <|
            \() ->
                Expect.equal
                    (Comment.aggregateComments [ informative, celebratory, actionable ])
                    (Summary "Check the comments for some suggestions.\u{202F}📣"
                        [ celebratory, actionable, informative, Comment.feedbackComment ]
                    )
        , test "no essential, no actionable, summary is informative" <|
            \() ->
                Expect.equal
                    (Comment.aggregateComments [ informative, celebratory ])
                    (Summary "Check the comments for some things to learn.\u{202F}📖"
                        [ celebratory, informative ]
                    )
        , test "no essential, no actionable, no informative, summary is celebratory" <|
            \() ->
                Expect.equal
                    (Comment.aggregateComments [ celebratory ])
                    (Summary "You're doing something right.\u{202F}🎉"
                        [ celebratory ]
                    )
        , test "order of comments in summary is fixed" <|
            \() ->
                Expect.equal
                    (Comment.aggregateComments [ essential, informative, celebratory, actionable, informative ])
                    (Summary "Check the comments for things to fix.\u{202F}🛠 "
                        [ celebratory, essential, actionable, informative, informative, Comment.feedbackComment ]
                    )
        , test "secondary order is alphabetical, but feedback is always last" <|
            \() ->
                Expect.equal
                    (Comment.aggregateComments
                        [ essential
                        , { essential | comment = "a" }
                        , informative
                        , { celebratory | comment = "a" }
                        , actionable
                        , celebratory
                        , { actionable | comment = "a" }
                        ]
                    )
                    (Summary "Check the comments for things to fix.\u{202F}🛠 "
                        [ celebratory
                        , { celebratory | comment = "a" }
                        , essential
                        , { essential | comment = "a" }
                        , actionable
                        , { actionable | comment = "a" }
                        , informative
                        , Comment.feedbackComment
                        ]
                    )
        ]


makeSummaryTest : Test
makeSummaryTest =
    describe "makeSummary should take in raw json output from elm-review and make a json summary"
        [ test "no comments" <|
            \() ->
                Comment.makeSummary [] "{\"type\":\"review-errors\",\"errors\":[]}"
                    |> Expect.equal (Ok "{\"summary\":\"No suggestions found.\",\"comments\":[]}")
        , test "exercise comment" <|
            \() ->
                Comment.makeSummary []
                    "{\"type\":\"review-errors\",\"errors\":[{\"path\":\"src/TwoFer.elm\",\"errors\":[{\"rule\":\"elm.two-fer.use_signature\",\"message\":\"{\\\"name\\\":\\\"has no signature\\\",\\\"comment\\\":\\\"elm.two-fer.use_signature\\\",\\\"type\\\":\\\"informative\\\",\\\"params\\\":{}}\",\"details\":[\"\"],\"region\":{\"start\":{\"line\":4,\"column\":1},\"end\":{\"line\":4,\"column\":7}},\"formatted\":[{\"string\":\"elm.two-fer.use_signature\",\"color\":\"#FF0000\"},\": {\\\"name\\\":\\\"has no signature\\\",\\\"comment\\\":\\\"elm.two-fer.use_signature\\\",\\\"type\\\":\\\"informative\\\",\\\"params\\\":{}}\\n\\n3| --twoFer : Maybe String -> String\\n4| twoFer name =\\n   \",{\"string\":\"^^^^^^\",\"color\":\"#FF0000\"},\"\\n5|     \\\"One for \\\"\\n\\n\"],\"suppressed\":false,\"originallySuppressed\":false}]}]}"
                    |> Expect.equal
                        (Ok
                            "{\"summary\":\"Check the comments for some things to learn.\u{202F}📖\",\"comments\":[{\"comment\":\"elm.two-fer.use_signature\",\"type\":\"informative\",\"params\":{}}]}"
                        )
        , test "common rule comment" <|
            \() ->
                Comment.makeSummary (List.concatMap RuleConfig.getDecoders [ NoUnused.ruleConfig, Simplify.ruleConfig ])
                    "{\"type\":\"review-errors\",\"errors\":[{\"path\":\"src/TwoFer.elm\",\"errors\":[{\"rule\":\"Simplify\",\"message\":\"Unnecessary concatenation with an empty string\",\"ruleLink\":\"https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/2.0.7/Simplify\",\"details\":[\"You should remove the concatenation with the empty string.\"],\"region\":{\"start\":{\"line\":6,\"column\":31},\"end\":{\"line\":6,\"column\":33}},\"fix\":[{\"range\":{\"start\":{\"line\":6,\"column\":31},\"end\":{\"line\":6,\"column\":37}},\"string\":\"\"}],\"formatted\":[{\"string\":\"(fix) \",\"color\":\"#33BBC8\"},{\"string\":\"Simplify\",\"color\":\"#FF0000\",\"href\":\"https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/2.0.7/Simplify\"},\": Unnecessary concatenation with an empty string\\n\\n5|     \\\"One for \\\"\\n6|         ++ Maybe.withDefault (\\\"\\\" ++ \\\"you\\\") name\\n                                 \",{\"string\":\"^^\",\"color\":\"#FF0000\"},\"\\n7|         ++ \\\", one for me.\\\"\\n\\nYou should remove the concatenation with the empty string.\"],\"suppressed\":false,\"originallySuppressed\":false}]}]}"
                    |> Expect.equal
                        (Ok
                            "{\"summary\":\"Check the comments for some suggestions.\u{202F}📣\",\"comments\":[{\"comment\":\"elm.common.simplify\",\"type\":\"actionable\",\"params\":{\"message\":\"Simplify: Unnecessary concatenation with an empty string\\n\\n5|     \\\"One for \\\"\\n6|         ++ Maybe.withDefault (\\\"\\\" ++ \\\"you\\\") name\\n                                 ^^\\n7|         ++ \\\", one for me.\\\"\\n\\nYou should remove the concatenation with the empty string.\"}},{\"comment\":\"elm.feedback_request\",\"type\":\"informative\",\"params\":{}}]}"
                        )
        , test "invalid input fails" <|
            \() ->
                Comment.makeSummary [] "I am invalid"
                    |> Expect.err
        , test "error from elm-review fails" <|
            \() ->
                Comment.makeSummary []
                    "{\"type\":\"error\",\"title\":\"ELM.JSON NOT FOUND\",\"path\":\"/Users/jie/Documents/exercism-repo/elm-analyzer/elm.json\",\"message\":[\"I could not find the elm.json of the project to review. I was looking for it at:\\n\\n    test_data/two-fer/perfect_solution/po/elm.json\\n\\nSince you specified this path, I’m assuming that you misconfigured the CLI’s\\narguments.\"]}"
                    |> Expect.err
        , test "common rule comment without decoder fails" <|
            \() ->
                Comment.makeSummary []
                    "{\"type\":\"review-errors\",\"errors\":[{\"path\":\"src/TwoFer.elm\",\"errors\":[{\"rule\":\"Simplify\",\"message\":\"Unnecessary concatenation with an empty string\",\"ruleLink\":\"https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/2.0.7/Simplify\",\"details\":[\"You should remove the concatenation with the empty string.\"],\"region\":{\"start\":{\"line\":6,\"column\":31},\"end\":{\"line\":6,\"column\":33}},\"fix\":[{\"range\":{\"start\":{\"line\":6,\"column\":31},\"end\":{\"line\":6,\"column\":37}},\"string\":\"\"}],\"formatted\":[{\"string\":\"(fix) \",\"color\":\"#33BBC8\"},{\"string\":\"Simplify\",\"color\":\"#FF0000\",\"href\":\"https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/2.0.7/Simplify\"},\": Unnecessary concatenation with an empty string\\n\\n5|     \\\"One for \\\"\\n6|         ++ Maybe.withDefault (\\\"\\\" ++ \\\"you\\\") name\\n                                 \",{\"string\":\"^^\",\"color\":\"#FF0000\"},\"\\n7|         ++ \\\", one for me.\\\"\\n\\nYou should remove the concatenation with the empty string.\"],\"suppressed\":false,\"originallySuppressed\":false}]}]}"
                    |> Expect.err
        ]
