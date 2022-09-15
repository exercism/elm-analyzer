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
        , CustomRule resetPlayerGoalCountMustUseInsert
        ]
    }


removeInsignificantPlayersMustUseFilter : Rule
removeInsignificantPlayersMustUseFilter =
    Analyzer.functionCalls
        { calledFrom = TopFunction "removeInsignificantPlayers"
        , findFunctions = [ FromExternalModule [ "Dict" ] "filter" ]
        , find = Some
        , comment = Comment "Doesn't use Dict.filter" "elm.top-scorers.use_filter" Essential Dict.empty
        }


resetPlayerGoalCountMustUseInsert : Rule
resetPlayerGoalCountMustUseInsert =
    Analyzer.functionCalls
        { calledFrom = TopFunction "resetPlayerGoalCount"
        , findFunctions = [ FromExternalModule [ "Dict" ] "insert" ]
        , find = Some
        , comment = Comment "Doesn't use Dict.insert" "elm.top-scorers.use_insert" Essential Dict.empty
        }


formatPlayersCannotUseSort : Rule
formatPlayersCannotUseSort =
    Analyzer.functionCalls
        { calledFrom = TopFunction "formatPlayers"
        , findFunctions = [ FromExternalModule [ "List" ] "sort" ]
        , find = None
        , comment = Comment "Uses List.sort" "elm.top-scorers.dont_use_sort" Essential Dict.empty
        }


combineGamesMustUseMerge : Rule
combineGamesMustUseMerge =
    Analyzer.functionCalls
        { calledFrom = TopFunction "combineGames"
        , findFunctions = [ FromExternalModule [ "Dict" ] "merge" ]
        , find = Some
        , comment = Comment "Doesn't use Dict.merge" "elm.top-scorers.use_merge" Essential Dict.empty
        }


aggregateScorersMustUseFoldl : Rule
aggregateScorersMustUseFoldl =
    Analyzer.functionCalls
        { calledFrom = TopFunction "aggregateScorers"
        , findFunctions = [ FromExternalModule [ "List" ] "foldl" ]
        , find = Some
        , comment = Comment "Doesn't use List.foldl" "elm.top-scorers.use_foldl" Essential Dict.empty
        }


aggregateScorersMustUseUpdateGoalCountForPlayer : Rule
aggregateScorersMustUseUpdateGoalCountForPlayer =
    Analyzer.functionCalls
        { calledFrom = TopFunction "aggregateScorers"
        , findFunctions = [ FromSameModule "updateGoalCountForPlayer" ]
        , find = Some
        , comment = Comment "Doesn't use updateGoalCountForPlayer" "elm.top-scorers.use_updateGoalCountForPlayer" Essential Dict.empty
        }



-- updateGoalCountForPlayer and List.foldl must be used in aggregateScorers
