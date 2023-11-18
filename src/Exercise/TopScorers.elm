module Exercise.TopScorers exposing (..)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/TopScorers.elm" ]
    , rules =
        [ CustomRule removeInsignificantPlayersMustUseFilter
            (Comment "Doesn't use Dict.filter" "elm.top-scorers.use_filter" Essential Dict.empty)
        , CustomRule resetPlayerGoalCountMustUseInsert
            (Comment "Doesn't use Dict.insert" "elm.top-scorers.use_insert" Essential Dict.empty)
        , CustomRule formatPlayersCannotUseSort
            (Comment "Uses List.sort" "elm.top-scorers.dont_use_sort" Essential Dict.empty)
        , CustomRule combineGamesMustUseMerge
            (Comment "Doesn't use Dict.merge" "elm.top-scorers.use_merge" Essential Dict.empty)
        , CustomRule aggregateScorersMustUseUpdateGoalCountForPlayer
            (Comment "Doesn't use updateGoalCountForPlayer" "elm.top-scorers.use_foldl_and_updateGoalCountForPlayer" Essential Dict.empty)
        , CustomRule aggregateScorersMustUseFold
            (Comment "Doesn't use List.foldl or List.foldr" "elm.top-scorers.use_foldl_and_updateGoalCountForPlayer" Essential Dict.empty)
        , CustomRule formatPlayerMustUseWithDefault
            (Comment "Doesn't use Maybe.withDefault" "elm.top-scorers.use_withDefault" Essential Dict.empty)
        ]
    }


removeInsignificantPlayersMustUseFilter : Comment -> Rule
removeInsignificantPlayersMustUseFilter =
    Analyzer.functionCalls
        { calledFrom = TopFunction "removeInsignificantPlayers"
        , findExpressions = [ FromExternalModule [ "Dict" ] "filter" ]
        , find = Some
        }


resetPlayerGoalCountMustUseInsert : Comment -> Rule
resetPlayerGoalCountMustUseInsert =
    Analyzer.functionCalls
        { calledFrom = TopFunction "resetPlayerGoalCount"
        , findExpressions = [ FromExternalModule [ "Dict" ] "insert" ]
        , find = Some
        }


formatPlayersCannotUseSort : Comment -> Rule
formatPlayersCannotUseSort =
    Analyzer.functionCalls
        { calledFrom = TopFunction "formatPlayers"
        , findExpressions = [ FromExternalModule [ "List" ] "sort" ]
        , find = None
        }


combineGamesMustUseMerge : Comment -> Rule
combineGamesMustUseMerge =
    Analyzer.functionCalls
        { calledFrom = TopFunction "combineGames"
        , findExpressions = [ FromExternalModule [ "Dict" ] "merge" ]
        , find = Some
        }


aggregateScorersMustUseUpdateGoalCountForPlayer : Comment -> Rule
aggregateScorersMustUseUpdateGoalCountForPlayer =
    Analyzer.functionCalls
        { calledFrom = TopFunction "aggregateScorers"
        , findExpressions = [ FromSameModule "updateGoalCountForPlayer" ]
        , find = All
        }


aggregateScorersMustUseFold : Comment -> Rule
aggregateScorersMustUseFold =
    Analyzer.functionCalls
        { calledFrom = TopFunction "aggregateScorers"
        , findExpressions = [ FromExternalModule [ "List" ] "foldl", FromExternalModule [ "List" ] "foldr" ]
        , find = Some
        }


formatPlayerMustUseWithDefault : Comment -> Rule
formatPlayerMustUseWithDefault =
    Analyzer.functionCalls
        { calledFrom = TopFunction "formatPlayer"
        , findExpressions = [ FromExternalModule [ "Maybe" ] "withDefault" ]
        , find = Some
        }
