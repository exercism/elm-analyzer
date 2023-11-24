module Exercise.LuciansLusciousLasagna exposing (reuseFunctions, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/LuciansLusciousLasagna.elm" ]
    , rules =
        [ CustomRule reuseFunctions
            (Comment "elm.lucians-luscious-lasagna.reuse_functions" Essential Dict.empty)
        ]
    }


reuseFunctions : Comment -> Rule
reuseFunctions =
    Analyzer.functionCalls
        { calledFrom = TopFunction "elapsedTimeInMinutes"
        , findExpressions = [ FromSameModule "preparationTimeInMinutes" ]
        , find = Some
        }
