module Exercise.ListOps exposing (doNotUseListModule, ruleConfig)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "list-ops"
    , restrictToFiles = Just [ "src/ListOps.elm" ]
    , rules =
        [ CustomRule doNotUseListModule
            (Comment "Uses the List module" "elm.list-ops.do_not_use_list" Essential Dict.empty)
        ]
    }


doNotUseListModule : Comment -> Rule
doNotUseListModule =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findFunctions = [ AnyFromExternalModule [ "List" ] ]
        , find = None
        }
