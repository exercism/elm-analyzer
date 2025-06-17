module Exercise.GithupApi exposing (decodeCommentUsesHelperFunctions, decodeCommentsUsesDecodeComment, decodeUserUsesIdAndNameHelpers, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/GithupApi.elm" ]
    , rules =
        [ CustomRule decodeUserUsesIdAndNameHelpers
            (Comment "elm.githup-api.decode_user_uses_id_and_name_helpers" Essential Dict.empty)
        , CustomRule decodeCommentUsesHelperFunctions
            (Comment "elm.githup-api.decode_comment_uses_helper_functions" Essential Dict.empty)
        , CustomRule decodeCommentsUsesDecodeComment
            (Comment "elm.githup-api.decode_comments_uses_decode_comment" Essential Dict.empty)
        ]
    }


decodeUserUsesIdAndNameHelpers : Comment -> Rule
decodeUserUsesIdAndNameHelpers =
    Analyzer.functionCalls
        { calledFrom = TopFunction "decodeUser"
        , findExpressions =
            [ FromSameModule "decodeId"
            , FromSameModule "decodeName"
            ]
        , find = All
        }


decodeCommentUsesHelperFunctions : Comment -> Rule
decodeCommentUsesHelperFunctions =
    Analyzer.functionCalls
        { calledFrom = TopFunction "decodeComment"
        , findExpressions =
            [ FromSameModule "decodeId"
            , FromSameModule "decodePullRequestReviewId"
            , FromSameModule "decodeUser"
            , FromSameModule "decodeSide"
            , FromSameModule "decodeLinks"
            ]
        , find = All
        }


decodeCommentsUsesDecodeComment : Comment -> Rule
decodeCommentsUsesDecodeComment =
    Analyzer.functionCalls
        { calledFrom = TopFunction "decodeComments"
        , findExpressions = [ FromSameModule "decodeComment" ]
        , find = All
        }
