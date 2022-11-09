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
            (Comment "maxPower doesn't use max" "elm.blorkemon-cards.use_max" Essential Dict.empty)
        , CustomRule sortByMonsterNameUsesSortBy
            (Comment "sortByMonsterName doesn't use List.sortBy" "elm.blorkemon-cards.use_sort_by" Essential Dict.empty)
        , CustomRule expectedWinnerUsesCompareShinyPower
            (Comment "expectedWinner doesn't use compareShinyPower" "elm.blorkemon-cards.use_shiny_power" Essential Dict.empty)
        , CustomRule expectedWinnerUsesCase
            (Comment "Doesn't use a case expression" "elm.blorkemon-cards.use_case" Essential Dict.empty)
        ]
    }


maxPowerUsesMax : Comment -> Rule
maxPowerUsesMax =
    Analyzer.functionCalls
        { calledFrom = TopFunction "maxPower"
        , findFunctions = [ FromExternalModule [ "Basics" ] "max" ]
        , find = Some
        }


sortByMonsterNameUsesSortBy : Comment -> Rule
sortByMonsterNameUsesSortBy =
    Analyzer.functionCalls
        { calledFrom = TopFunction "sortByMonsterName"
        , findFunctions = [ FromExternalModule [ "List" ] "sortBy" ]
        , find = Some
        }


expectedWinnerUsesCompareShinyPower : Comment -> Rule
expectedWinnerUsesCompareShinyPower =
    Analyzer.functionCalls
        { calledFrom = TopFunction "expectedWinner"
        , findFunctions = [ FromSameModule "compareShinyPower" ]
        , find = Some
        }


expectedWinnerUsesCase : Comment -> Rule
expectedWinnerUsesCase =
    Analyzer.functionCalls
        { calledFrom = TopFunction "expectedWinner"
        , findFunctions = [ CaseBlock ]
        , find = Some
        }
