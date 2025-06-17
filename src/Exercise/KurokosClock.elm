module Exercise.KurokosClock exposing (ruleConfig, usesShowLocalDateAndShowLocalTime)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/KurokosClock.elm" ]
    , rules =
        [ CustomRule usesShowLocalDateAndShowLocalTime (Comment "elm.kurokos-clock.use_showLocalDate_and_showLocalTime" Essential Dict.empty)
        ]
    }


usesShowLocalDateAndShowLocalTime : Comment -> Rule
usesShowLocalDateAndShowLocalTime =
    Analyzer.functionCalls
        { calledFrom = TopFunction "showDateTime"
        , findExpressions =
            [ FromSameModule "showLocalDate"
            , FromSameModule "showLocalTime"
            ]
        , find = All
        }
