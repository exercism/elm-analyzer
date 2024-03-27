module Exercise.Sieve exposing (doNotUseDivisionOrModulo, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/Sieve.elm" ]
    , rules =
        [ CustomRule doNotUseDivisionOrModulo
            (Comment "elm.sieve.do_not_use_division_or_modulo" Essential Dict.empty)
        ]
    }


doNotUseDivisionOrModulo : Comment -> Rule
doNotUseDivisionOrModulo =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findExpressions =
            [ Operator "//"
            , Operator "/"
            , FromExternalModule [ "Basics" ] "modBy"
            , FromExternalModule [ "Basics" ] "remainderBy"
            ]
        , find = None
        }
