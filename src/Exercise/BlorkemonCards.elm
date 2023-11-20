module Exercise.BlorkemonCards exposing (..)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/BlorkemonCards.elm" ]
    , rules =
        [ CustomRule maxPowerUsesMax
            (Comment "elm.blorkemon-cards.use_max" Essential Dict.empty)
        , CustomRule sortByMonsterNameUsesSortBy
            (Comment "elm.blorkemon-cards.use_sort_by" Essential Dict.empty)
        , CustomRule expectedWinnerUsesCompareShinyPower
            (Comment "elm.blorkemon-cards.use_shiny_power" Essential Dict.empty)
        , CustomRule expectedWinnerUsesCase
            (Comment "elm.blorkemon-cards.use_case" Essential Dict.empty)
        ]
    }


maxPowerUsesMax : Comment -> Rule
maxPowerUsesMax =
    Analyzer.functionCalls
        { calledFrom = TopFunction "maxPower"
        , findExpressions = [ FromExternalModule [ "Basics" ] "max" ]
        , find = Some
        }


sortByMonsterNameUsesSortBy : Comment -> Rule
sortByMonsterNameUsesSortBy =
    Analyzer.functionCalls
        { calledFrom = TopFunction "sortByMonsterName"
        , findExpressions = [ FromExternalModule [ "List" ] "sortBy" ]
        , find = Some
        }


expectedWinnerUsesCompareShinyPower : Comment -> Rule
expectedWinnerUsesCompareShinyPower =
    Analyzer.functionCalls
        { calledFrom = TopFunction "expectedWinner"
        , findExpressions = [ FromSameModule "compareShinyPower" ]
        , find = Some
        }


expectedWinnerUsesCase : Comment -> Rule
expectedWinnerUsesCase =
    Analyzer.functionCalls
        { calledFrom = TopFunction "expectedWinner"
        , findExpressions = [ CaseBlock ]
        , find = Some
        }
