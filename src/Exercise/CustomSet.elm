module Exercise.CustomSet exposing (doNotUseSetModule, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/CustomSet.elm" ]
    , rules =
        [ CustomRule doNotUseSetModule
            (Comment "elm.custom-set.do_not_use_set" Essential Dict.empty)
        ]
    }


doNotUseSetModule : Comment -> Rule
doNotUseSetModule =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findExpressions = [ AnyFromExternalModule [ "Set" ] ]
        , find = None
        }
