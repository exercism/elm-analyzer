module Exercise.Secrets exposing (ruleConfig, usesAllHelperFunctions)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/Secrets.elm" ]
    , rules =
        [ CustomRule usesAllHelperFunctions (Comment "elm.secrets.use_all_helper_functions" Essential Dict.empty)
        ]
    }


usesAllHelperFunctions : Comment -> Rule
usesAllHelperFunctions =
    Analyzer.functionCalls
        { calledFrom = TopFunction "decrypt"
        , findExpressions =
            [ FromSameModule "shiftBack"
            , FromSameModule "setBits"
            , FromSameModule "flipBits"
            , FromSameModule "clearBits"
            ]
        , find = All
        }
