module Exercise.TicketPleaseTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TicketPlease as TicketPlease
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "TicketPleaseTest"
        [ exemplar
        , emptyCommentArguments
        , numberOfCreatorCommentsArguments
        , numberOfCreatorCommentsExpressions
        , assignedToDevTeamArguments
        , assignedToDevTeamExpressions
        , assignTicketToArguments
        , assignTicketIgnoresCases
        , assignTicketRecordUpdate
        ]


rules : List Rule
rules =
    TicketPlease.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module TicketPlease exposing (..)

import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))

emptyComment : ( User, String ) -> Bool
emptyComment ( _, comment ) =
    String.isEmpty comment

numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket { createdBy, comments }) =
    let
        ( creator, _ ) =
            createdBy
    in
    List.length
        (List.filter (\\( user, _ ) -> user == creator) comments)

assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket { assignedTo }) =
    case assignedTo of
        Just (User "Alice") ->
            True

        Just (User "Bob") ->
            True

        Just (User "Charlie") ->
            True

        _ ->
            False

assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user (Ticket ({ status } as ticket)) =
    case status of
        New ->
            Ticket { ticket | status = InProgress, assignedTo = Just user }

        Archived ->
            Ticket ticket

        _ ->
            Ticket { ticket | assignedTo = Just user }
"""


emptyCommentArguments : Test
emptyCommentArguments =
    let
        comment =
            Comment "emptyComment argument doesn't destructure a tuple and wild card" "elm.ticket-please.destructure_emptyComment_argument" Essential Dict.empty
    in
    describe "emptyComment argument doesn't destructure a tuple and wild card"
        [ test "doesn't ignore the user" <|
            \() ->
                """
module TicketPlease exposing (..)

emptyComment : ( User, String ) -> Bool
emptyComment ( user, comment ) =
    String.isEmpty comment

"""
                    |> Review.Test.run (TicketPlease.emptyCommentArgumentShouldUseTupleAndIgnore comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "emptyComment"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 13 } }
                        ]
        , test "doesn't destructure the tuple" <|
            \() ->
                """
module TicketPlease exposing (..)

emptyComment : ( User, String ) -> Bool
emptyComment =
    Tuple.second >> String.isEmpty

"""
                    |> Review.Test.run (TicketPlease.emptyCommentArgumentShouldUseTupleAndIgnore comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "emptyComment"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 13 } }
                        ]
        ]


numberOfCreatorCommentsArguments : Test
numberOfCreatorCommentsArguments =
    let
        comment =
            Comment "numberOfCreatorComments argument doesn't destructure a record in a named pattern" "elm.ticket-please.destructure_numberOfCreatorComments_argument" Essential Dict.empty
    in
    describe "numberOfCreatorComments argument doesn't destructure a record in a named pattern"
        [ test "doesn't destructure the record" <|
            \() ->
                """
module TicketPlease exposing (..)

numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket ticket) =
    let
        ( creator, _ ) =
            ticket.createdBy
    in
    List.length
        (List.filter (\\( user, _ ) -> user == creator) ticket.comments)
"""
                    |> Review.Test.run (TicketPlease.numberOfCreatorCommentsArgumentShouldUseNamedAndRecord comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "numberOfCreatorComments"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                        ]
        , test "doesn't destructure the Ticket in the argument" <|
            \() ->
                """
module TicketPlease exposing (..)

numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments ticket =
    let
        (Ticket { createdBy, comments }) =
            ticket

        ( creator, _ ) =
            createdBy
    in
    List.length
        (List.filter (\\( user, _ ) -> user == creator) comments)
"""
                    |> Review.Test.run (TicketPlease.numberOfCreatorCommentsArgumentShouldUseNamedAndRecord comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "numberOfCreatorComments"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                        ]
        ]


numberOfCreatorCommentsExpressions : Test
numberOfCreatorCommentsExpressions =
    let
        comment =
            Comment "numberOfCreatorComments doesn't destructure in let and lambda" "elm.ticket-please.destructure_numberOfCreatorComments_expressions" Essential Dict.empty
    in
    describe "numberOfCreatorComments doesn't destructure in let and lambda"
        [ test "doesn't destructure in the let" <|
            \() ->
                """
module TicketPlease exposing (..)

numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket { createdBy, comments }) =
    List.length
        (List.filter (\\( user, _ ) -> user == Tuple.first creator) comments)
"""
                    |> Review.Test.run (TicketPlease.numberOfCreatorCommentsDestructuresTupleInLetAndLambda comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "numberOfCreatorComments"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                        ]
        , test "doesn't destructure in lambda" <|
            \() ->
                """
module TicketPlease exposing (..)

numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket { createdBy, comments }) =
    let
        ( creator, _ ) =
            createdBy
    in
    List.length
        (List.filter (\\user -> Tuple.first user == creator) comments)

"""
                    |> Review.Test.run (TicketPlease.numberOfCreatorCommentsDestructuresTupleInLetAndLambda comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "numberOfCreatorComments"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                        ]
        ]


assignedToDevTeamArguments : Test
assignedToDevTeamArguments =
    let
        comment =
            Comment "assignedToDevTeam argument doesn't destructure a record in a named pattern" "elm.ticket-please.destructure_assignedToDevTeam_argument" Essential Dict.empty
    in
    describe "assignedToDevTeam argument doesn't destructure a record in a named pattern"
        [ test "doesn't destructure the record" <|
            \() ->
                """
module TicketPlease exposing (..)

assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket ticket) =
    case ticket.assignedTo of
        Just (User "Alice") ->
            True

        Just (User "Bob") ->
            True

        Just (User "Charlie") ->
            True

        _ ->
            False
"""
                    |> Review.Test.run (TicketPlease.assignedToDevTeamArgumentShouldUseNamedAndRecord comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "assignedToDevTeam"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 18 } }
                        ]
        , test "doesn't destructure the Ticket in the argument" <|
            \() ->
                """
module TicketPlease exposing (..)

assignedToDevTeam : Ticket -> Bool
assignedToDevTeam ticket =
    let
        (Ticket { assignedTo }) = ticket
    in
    case assignedTo of
        Just (User "Alice") ->
            True

        Just (User "Bob") ->
            True

        Just (User "Charlie") ->
            True

        _ ->
            False
"""
                    |> Review.Test.run (TicketPlease.assignedToDevTeamArgumentShouldUseNamedAndRecord comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "assignedToDevTeam"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 18 } }
                        ]
        ]


assignedToDevTeamExpressions : Test
assignedToDevTeamExpressions =
    let
        comment =
            Comment "assignedToDevTeam argument doesn't destructure in a case block" "elm.ticket-please.destructure_assignedToDevTeam_expressions" Essential Dict.empty
    in
    describe "assignedToDevTeam argument doesn't destructure in a case block"
        [ test "doesn't destructure the string" <|
            \() ->
                """
module TicketPlease exposing (..)

assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket { assignedTo }) =
    case assignedTo of
        Just (User user) ->
            user == "Alice || user == "Bob" || user == "Charlie

        _ ->
            False
"""
                    |> Review.Test.run (TicketPlease.assignedToDevTeamArgumentDestructureInCase comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "assignedToDevTeam"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 18 } }
                        ]
        , test "doesn't destructure Just or User" <|
            \() ->
                """
module TicketPlease exposing (..)

assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket { assignedTo }) =
    let
        (User name) = Maybe.withDefault (User "") assignedTo
    in
    case name of
        "Alice" ->
            True

        "Bob" ->
            True

        "Charlie" ->
            True

        _ ->
            False
"""
                    |> Review.Test.run (TicketPlease.assignedToDevTeamArgumentDestructureInCase comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "assignedToDevTeam"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 18 } }
                        ]
        ]


assignTicketToArguments : Test
assignTicketToArguments =
    let
        comment =
            Comment "assignTicketTo argument doesn't destructure a record in a named pattern using as" "elm.ticket-please.destructure_assignTicketTo_argument" Essential Dict.empty
    in
    describe "assignTicketTo argument doesn't destructure a record in a named pattern using as"
        [ test "doesn't destructure the record" <|
            \() ->
                """
module TicketPlease exposing (..)

assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user (Ticket ticket) =
    case ticket.status of
        New ->
            Ticket { ticket | status = InProgress, assignedTo = Just user }

        Archived ->
            Ticket ticket

        _ ->
            Ticket { ticket | assignedTo = Just user }
"""
                    |> Review.Test.run (TicketPlease.assignTicketToArgumentShouldUseNamedRecordAndAs comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "assignTicketTo"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                        ]
        , test "doesn't destructure the Ticket in the argument" <|
            \() ->
                """
module TicketPlease exposing (..)

assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user ticketParam =
    let
        (Ticket ({ status } as ticket)) = ticketParam
    in
    case status of
        New ->
            Ticket { ticket | status = InProgress, assignedTo = Just user }

        Archived ->
            Ticket ticket

        _ ->
            Ticket { ticket | assignedTo = Just user }
"""
                    |> Review.Test.run (TicketPlease.assignTicketToArgumentShouldUseNamedRecordAndAs comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "assignTicketTo"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                        ]
        , test "doesn't use as in the argument" <|
            \() ->
                """
module TicketPlease exposing (..)

assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user (Ticket ({ status, createdBy , assignedTo , comments })) =
    case status of
        New ->
            Ticket { status = InProgress, assignedTo = Just user, createdBy = createdBy, comments = comment }

        Archived ->
            Ticket  { status = status, assignedTo = assignedTo, createdBy = createdBy, comments = comment }

        _ ->
            Ticket { status = status, assignedTo = Just user, createdBy = createdBy, comments = comment }
"""
                    |> Review.Test.run (TicketPlease.assignTicketToArgumentShouldUseNamedRecordAndAs comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "assignTicketTo"
                            |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                        ]
        ]


assignTicketIgnoresCases : Test
assignTicketIgnoresCases =
    let
        comment =
            Comment "assignTicketTo argument doesn't ignore cases" "elm.ticket-please.assignTicketTo_ignore_cases" Actionable Dict.empty
    in
    test "assignTicketTo argument doesn't ignore cases" <|
        \() ->
            """
module TicketPlease exposing (..)

assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user (Ticket ({ status } as ticket)) =
    case status of
        New ->
            Ticket { ticket | status = InProgress, assignedTo = Just user }

        Archived ->
            Ticket ticket

        Closed ->
            Ticket { ticket | assignedTo = Just user }

        InProgress ->
            Ticket { ticket | assignedTo = Just user }

        Resolved ->
            Ticket { ticket | assignedTo = Just user }
"""
                |> Review.Test.run (TicketPlease.assignTicketToArgumentShouldUseIgnore comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "assignTicketTo"
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                    ]


assignTicketRecordUpdate : Test
assignTicketRecordUpdate =
    let
        comment =
            Comment "assignTicketTo doesn't use the record update syntax" "elm.ticket-please.assignTicketTo_use_record_update" Actionable Dict.empty
    in
    test "assignTicketTo doesn't use the record update syntax" <|
        \() ->
            """
module TicketPlease exposing (..)

assignTicketTo user (Ticket ({ status, createdBy , assignedTo , comments } as ticket)) =
    case status of
        New ->
            Ticket { status = InProgress, assignedTo = Just user, createdBy = createdBy, comments = comment }

        Archived ->
            Ticket  { status = status, assignedTo = assignedTo, createdBy = createdBy, comments = comment }

        _ ->
            Ticket { status = status, assignedTo = Just user, createdBy = createdBy, comments = comment }
"""
                |> Review.Test.run (TicketPlease.assignTicketUsesRecordUpdate comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "assignTicketTo"
                        |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } }
                    ]
