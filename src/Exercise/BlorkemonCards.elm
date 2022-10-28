module Exercise.BlorkemonCards exposing (..)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "blorkemon-cards"
    , restrictToFiles = Just [ "src/BlorkemonCards.elm" ]
    , rules =
        [ CustomRule maxPowerUsesMax
        , CustomRule sortByMonsterNameUsesSortBy
        , CustomRule expectedWinnerUsesCompareShinyPower
        , CustomRule expectedWinnerUsesCase
        ]
    }


maxPowerUsesMax : Rule
maxPowerUsesMax =
    Analyzer.functionCalls
        { calledFrom = TopFunction "maxPower"
        , findFunctions = [ FromExternalModule [ "Basics" ] "max" ]
        , find = Some
        , comment = Comment "maxPower doesn't use max" "elm.blorkemon-cards.use_max" Essential Dict.empty
        }


sortByMonsterNameUsesSortBy : Rule
sortByMonsterNameUsesSortBy =
    Analyzer.functionCalls
        { calledFrom = TopFunction "sortByMonsterName"
        , findFunctions = [ FromExternalModule [ "List" ] "sortBy" ]
        , find = Some
        , comment = Comment "sortByMonsterName doesn't use List.sortBy" "elm.blorkemon-cards.use_sort_by" Essential Dict.empty
        }


expectedWinnerUsesCompareShinyPower : Rule
expectedWinnerUsesCompareShinyPower =
    Analyzer.functionCalls
        { calledFrom = TopFunction "expectedWinner"
        , findFunctions = [ FromSameModule "compareShinyPower" ]
        , find = Some
        , comment = Comment "expectedWinner doesn't use compareShinyPower" "elm.blorkemon-cards.use_shiny_power" Essential Dict.empty
        }


expectedWinnerUsesCase : Rule
expectedWinnerUsesCase =
    Analyzer.functionCalls
        { calledFrom = TopFunction "expectedWinner"
        , findFunctions = [ CaseBlock ]
        , find = Some
        , comment = Comment "Doesn't use a case expression" "elm.blorkemon-cards.use_case" Essential Dict.empty
        }
