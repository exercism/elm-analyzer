module Exercise.Sieve exposing (doNotUseDivision, doNotUseModulo, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/Sieve.elm" ]
    , rules =
        [ CustomRule doNotUseDivision
            (Comment "elm.sieve.do_not_use_division" Essential Dict.empty)
        , CustomRule doNotUseModulo
            (Comment "elm.sieve.do_not_use_modulo" Essential Dict.empty)
        ]
    }


doNotUseDivision : Comment -> Rule
doNotUseDivision =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findExpressions = [ Operator "//", Operator "/" ]
        , find = None
        }


doNotUseModulo : Comment -> Rule
doNotUseModulo =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findExpressions = [ FromExternalModule [ "Basics" ] "modBy", FromExternalModule [ "Basics" ] "remainderBy" ]
        , find = None
        }
