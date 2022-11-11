module Exercise.Strain exposing (doNotUseFilter, ruleConfig)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "strain"
    , restrictToFiles = Just [ "src/Strain.elm" ]
    , rules =
        [ CustomRule doNotUseFilter
            (Comment "Uses the List module" "elm.strain.do_not_use_filter" Essential Dict.empty)
        ]
    }


doNotUseFilter : Comment -> Rule
doNotUseFilter =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findFunctions = [ FromExternalModule [ "List" ] "filter", FromExternalModule [ "List" ] "filterMap" ]
        , find = None
        }
