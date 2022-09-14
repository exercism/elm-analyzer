module Exercise.TopScorers exposing (..)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "top-scorers"
    , restrictToFiles = Just [ "src/TopScorers.elm" ]
    , rules =
        [ CustomRule removeInsignificantPlayersMustUseFilter
        ]
    }


removeInsignificantPlayersMustUseFilter : Rule
removeInsignificantPlayersMustUseFilter =
    Analyzer.functionCalls
        { calledFrom = TopFunction "removeInsignificantPlayers"
        , findFunctions = [ FromExternalModule [ "Dict" ] "filter" ]
        , find = Some
        , comment = Comment "Uses the Dict module" "elm.top-scorers.use_filter" Essential Dict.empty
        }
