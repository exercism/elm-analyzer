module Exercise.ValentinesDay exposing (ruleConfig, usesCase)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "valentines-day"
    , restrictToFiles = Just [ "src/ValentinesDay.elm" ]
    , rules =
        [ CustomRule usesCase
            (Comment "Doesn't use a case expression" "elm.valentines-day.use_case_statement" Essential Dict.empty)
        ]
    }


usesCase : Comment -> Rule
usesCase =
    Analyzer.functionCalls
        { calledFrom = TopFunction "rateActivity"
        , findExpressions = [ CaseBlock ]
        , find = Some
        }
