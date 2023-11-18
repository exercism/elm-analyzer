module Exercise.Strain exposing (doNotUseFilter, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/Strain.elm" ]
    , rules =
        [ CustomRule doNotUseFilter
            (Comment "elm.strain.do_not_use_filter" Essential Dict.empty)
        ]
    }


doNotUseFilter : Comment -> Rule
doNotUseFilter =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findExpressions = [ FromExternalModule [ "List" ] "filter", FromExternalModule [ "List" ] "filterMap" ]
        , find = None
        }
