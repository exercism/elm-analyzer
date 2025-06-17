module Exercise.GithupApiTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.GithupApi as GithupApi
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "GithupApiTest"
        [ exemplar
        , decodeUserDoesNotUseIdAndNameHelpers
        , decodeCommentDoesNotUseHelperFunctions
        , decodeCommentsDoesNotUseDecodeComment
        ]


rules : List Rule
rules =
    GithupApi.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module GithupApi exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias User =
    { id : Int
    , name : Maybe String
    , login : String
    , avatarUrl : String
    , siteAdmin : Bool
    }


type Side
    = Left
    | Right


type alias Comment =
    { id : Int
    , pullRequestReviewId : Maybe Int
    , user : User
    , body : String
    , side : Side
    , links : Dict String String
    }


decodeId : Decoder Int
decodeId =
    Decode.field "id" Decode.int


decodeName : Decoder (Maybe String)
decodeName =
    Decode.oneOf
        [ Decode.field "name" Decode.string |> Decode.map Just
        , Decode.succeed Nothing
        ]


decodeUser : Decoder User
decodeUser =
    Decode.map5 User
        decodeId
        decodeName
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "site_admin" Decode.bool)


decodePullRequestReviewId : Decoder (Maybe Int)
decodePullRequestReviewId =
    Decode.field "pull_request_review_id" (Decode.nullable Decode.int)


decodeSide : Decoder Side
decodeSide =
    Decode.string
        |> Decode.andThen
            (\\side ->
                case side of
                    "LEFT" ->
                        Decode.succeed Left

                    "RIGHT" ->
                        Decode.succeed Right

                    _ ->
                        Decode.fail "not LEFT or RIGHT"
            )
        |> Decode.field "side"


decodeLinks : Decoder (Dict String String)
decodeLinks =
    Decode.dict (Decode.field "href" Decode.string)
        |> Decode.field "_links"


decodeComment : Decoder Comment
decodeComment =
    Decode.map6 Comment
        decodeId
        decodePullRequestReviewId
        (Decode.field "user" decodeUser)
        (Decode.field "body" Decode.string)
        decodeSide
        decodeLinks


decodeComments : Decoder (List Comment)
decodeComments =
    Decode.list decodeComment


encodeComment : Comment -> Value
encodeComment comment =
    let
        maybeEncodeName maybeName =
            case maybeName of
                Nothing ->
                    []

                Just name ->
                    [ ( "name", Encode.string name ) ]

        encodeUser user =
            Encode.object
                ([ ( "id", Encode.int user.id )
                 , ( "login", Encode.string user.login )
                 , ( "avatar_url", Encode.string user.avatarUrl )
                 , ( "site_admin", Encode.bool user.siteAdmin )
                 ]
                    ++ maybeEncodeName user.name
                )

        encodeSide side =
            case side of
                Left ->
                    Encode.string "LEFT"

                Right ->
                    Encode.string "RIGHT"
    in
    Encode.object
        [ ( "id", Encode.int comment.id )
        , ( "pull_request_review_id"
          , comment.pullRequestReviewId |> Maybe.map Encode.int |> Maybe.withDefault Encode.null
          )
        , ( "user", encodeUser comment.user )
        , ( "body", Encode.string comment.body )
        , ( "side", encodeSide comment.side )
        , ( "_links", Encode.dict identity (\\url -> Encode.object [ ( "href", Encode.string url ) ]) comment.links )
        ]
"""


decodeUserDoesNotUseIdAndNameHelpers : Test
decodeUserDoesNotUseIdAndNameHelpers =
    let
        comment =
            Comment "elm.githup-api.decode_user_uses_required_id_and_name_helpers" Essential Dict.empty
    in
    test "decodeUser doesn't use decodeId and decodeName" <|
        \() ->
            """
module GithupApi exposing (decodeUser, decodeId, decodeName)

import Json.Decode as Decode exposing (Decoder, field, string, int)

decodeUser : Decoder User
decodeUser =
    Decode.map5 User
        (Decode.field "id" Decode.int)
        (Decode.oneOf
            [ Decode.field "name" Decode.string |> Decode.map Just
            , Decode.succeed Nothing
            ]
        )
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "site_admin" Decode.bool)
"""
                |> Review.Test.run (GithupApi.decodeUserUsesIdAndNameHelpers comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "decodeUser"
                        |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 11 } }
                    ]


decodeCommentDoesNotUseHelperFunctions : Test
decodeCommentDoesNotUseHelperFunctions =
    let
        comment =
            Comment "elm.githup-api.decode_comment_uses_required_functions" Essential Dict.empty
    in
    test "decodeComment doesn't use required functions" <|
        \() ->
            """
module GithupApi exposing (decodeComment, decodeId, decodeName, decodePullRequestReviewId, decodeSide, decodeLinks, decodeUser)

import Json.Decode as Decode exposing (Decoder, field, string, int)

decodeComment : Decoder Comment
decodeComment =
    Decode.map6 Comment
        (Decode.field "id" Decode.int)
        (Decode.field "pull_request_review_id" (Decode.nullable Decode.int))
        (Decode.field "user"
            (Decode.map5 User
                (Decode.field "id" Decode.int)
                (Decode.oneOf
                    [ Decode.field "name" Decode.string |> Decode.map Just
                    , Decode.succeed Nothing
                    ]
                )
                (Decode.field "login" Decode.string)
                (Decode.field "avatar_url" Decode.string)
                (Decode.field "site_admin" Decode.bool)
            )
        )
        (Decode.field "body" Decode.string)
        (Decode.string
            |> Decode.andThen
                (\\side ->
                    case side of
                        "LEFT" ->
                            Decode.succeed Left

                        "RIGHT" ->
                            Decode.succeed Right

                        _ ->
                            Decode.fail "not LEFT or RIGHT"
                )
            |> Decode.field "side"
        )
        (Decode.dict (Decode.field "href" Decode.string)
            |> Decode.field "_links"
        )
"""
                |> Review.Test.run (GithupApi.decodeCommentUsesHelperFunctions comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "decodeComment"
                        |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 14 } }
                    ]


decodeCommentsDoesNotUseDecodeComment : Test
decodeCommentsDoesNotUseDecodeComment =
    let
        comment =
            Comment "elm.githup-api.decode_comments_uses_decode_comment" Essential Dict.empty
    in
    test "decodeComments doesn't use decodeComment" <|
        \() ->
            """
module GithupApi exposing (decodeComments, decodeComment, decodeId, decodeName, decodePullRequestReviewId, decodeSide, decodeLinks, decodeUser)

import Json.Decode as Decode exposing (Decoder, field, list, string, int)

decodeComments : Decoder (List Comment)
decodeComments =
    Decode.map6 Comment
        (Decode.field "id" Decode.int)
        (Decode.field "pull_request_review_id" (Decode.nullable Decode.int))
        (Decode.field "user"
            (Decode.map5 User
                (Decode.field "id" Decode.int)
                (Decode.oneOf
                    [ Decode.field "name" Decode.string |> Decode.map Just
                    , Decode.succeed Nothing
                    ]
                )
                (Decode.field "login" Decode.string)
                (Decode.field "avatar_url" Decode.string)
                (Decode.field "site_admin" Decode.bool)
            )
        )
        (Decode.field "body" Decode.string)
        (Decode.string
            |> Decode.andThen
                (\\side ->
                    case side of
                        "LEFT" ->
                            Decode.succeed Left

                        "RIGHT" ->
                            Decode.succeed Right

                        _ ->
                            Decode.fail "not LEFT or RIGHT"
                )
            |> Decode.field "side"
        )
        decodeLinks
        |> Decode.list
"""
                |> Review.Test.run (GithupApi.decodeCommentsUsesDecodeComment comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "decodeComments"
                        |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 15 } }
                    ]
