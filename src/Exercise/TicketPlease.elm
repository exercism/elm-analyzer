module Exercise.TicketPlease exposing
    ( assignTicketToArgumentShouldUseIgnore
    , assignTicketToArgumentShouldUseNamedRecordAndAs
    , assignTicketUsesRecordUpdate
    , assignedToDevTeamArgumentDestructureInCase
    , assignedToDevTeamArgumentShouldUseNamedAndRecord
    , emptyCommentArgumentShouldUseTupleAndIgnore
    , numberOfCreatorCommentsArgumentShouldUseNamedAndRecord
    , numberOfCreatorCommentsDestructuresTupleInLetAndLambda
    , ruleConfig
    )

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..), Pattern(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Pattern exposing (Pattern(..))
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/TicketPlease.elm" ]
    , rules =
        [ CustomRule emptyCommentArgumentShouldUseTupleAndIgnore
            (Comment "elm.ticket-please.destructure_emptyComment_argument" Essential Dict.empty)
        , CustomRule numberOfCreatorCommentsArgumentShouldUseNamedAndRecord
            (Comment "elm.ticket-please.destructure_numberOfCreatorComments_argument" Essential Dict.empty)
        , CustomRule numberOfCreatorCommentsDestructuresTupleInLetAndLambda
            (Comment "elm.ticket-please.destructure_numberOfCreatorComments_expressions" Essential Dict.empty)
        , CustomRule assignedToDevTeamArgumentShouldUseNamedAndRecord
            (Comment "elm.ticket-please.destructure_assignedToDevTeam_argument" Essential Dict.empty)
        , CustomRule assignedToDevTeamArgumentDestructureInCase
            (Comment "elm.ticket-please.destructure_assignedToDevTeam_expressions" Essential Dict.empty)
        , CustomRule assignTicketToArgumentShouldUseNamedRecordAndAs
            (Comment "elm.ticket-please.destructure_assignTicketTo_argument" Essential Dict.empty)
        , CustomRule assignTicketToArgumentShouldUseIgnore
            (Comment "elm.ticket-please.assignTicketTo_ignore_cases" Actionable Dict.empty)
        , CustomRule assignTicketUsesRecordUpdate
            (Comment "elm.ticket-please.assignTicketTo_use_record_update" Actionable Dict.empty)
        ]
    }


emptyCommentArgumentShouldUseTupleAndIgnore : Comment -> Rule
emptyCommentArgumentShouldUseTupleAndIgnore =
    Analyzer.functionCalls
        { calledFrom = TopFunction "emptyComment"
        , findExpressions = [ ArgumentWithPattern Tuple, ArgumentWithPattern Ignore ]
        , find = All
        }


numberOfCreatorCommentsArgumentShouldUseNamedAndRecord : Comment -> Rule
numberOfCreatorCommentsArgumentShouldUseNamedAndRecord =
    Analyzer.functionCalls
        { calledFrom = TopFunction "numberOfCreatorComments"
        , findExpressions = [ ArgumentWithPattern Named, ArgumentWithPattern Record ]
        , find = All
        }


numberOfCreatorCommentsDestructuresTupleInLetAndLambda : Comment -> Rule
numberOfCreatorCommentsDestructuresTupleInLetAndLambda =
    Analyzer.functionCalls
        { calledFrom = TopFunction "numberOfCreatorComments"
        , findExpressions = [ LetWithPattern Tuple, LambdaWithPattern Tuple ]
        , find = All
        }


assignedToDevTeamArgumentShouldUseNamedAndRecord : Comment -> Rule
assignedToDevTeamArgumentShouldUseNamedAndRecord =
    Analyzer.functionCalls
        { calledFrom = TopFunction "assignedToDevTeam"
        , findExpressions = [ ArgumentWithPattern Named, ArgumentWithPattern Record ]
        , find = All
        }


assignedToDevTeamArgumentDestructureInCase : Comment -> Rule
assignedToDevTeamArgumentDestructureInCase =
    Analyzer.functionCalls
        { calledFrom = TopFunction "assignedToDevTeam"
        , findExpressions = [ CaseWithPattern Named, CaseWithPattern String ]
        , find = All
        }


assignTicketToArgumentShouldUseNamedRecordAndAs : Comment -> Rule
assignTicketToArgumentShouldUseNamedRecordAndAs =
    Analyzer.functionCalls
        { calledFrom = TopFunction "assignTicketTo"
        , findExpressions = [ ArgumentWithPattern Named, ArgumentWithPattern Record, ArgumentWithPattern As ]
        , find = All
        }


assignTicketToArgumentShouldUseIgnore : Comment -> Rule
assignTicketToArgumentShouldUseIgnore =
    Analyzer.functionCalls
        { calledFrom = TopFunction "assignTicketTo"
        , findExpressions = [ CaseWithPattern Ignore ]
        , find = All
        }


assignTicketUsesRecordUpdate : Comment -> Rule
assignTicketUsesRecordUpdate =
    Analyzer.functionCalls
        { calledFrom = TopFunction "assignTicketTo"
        , findExpressions = [ RecordUpdate ]
        , find = All
        }
