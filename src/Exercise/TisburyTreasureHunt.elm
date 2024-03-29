module Exercise.TisburyTreasureHunt exposing (countPlaceTreasuresUsesTupleSecond, ruleConfig, specialCaseSwapPossibleShouldTupleInCase, treasureLocationMatchesPlaceLocationUsesPlaceLocationToTreasureLocation)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..), Pattern(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Elm.Syntax.Pattern exposing (Pattern(..))
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/TisburyTreasureHunt.elm" ]
    , rules =
        [ CustomRule specialCaseSwapPossibleShouldTupleInCase
            (Comment "elm.tisbury-treasure-hunt.use_tuple_in_case" Essential Dict.empty)
        , CustomRule treasureLocationMatchesPlaceLocationUsesPlaceLocationToTreasureLocation
            (Comment "elm.tisbury-treasure-hunt.use_placeLocationToTreasureLocation" Essential Dict.empty)
        , CustomRule countPlaceTreasuresUsesTupleSecond
            (Comment "elm.tisbury-treasure-hunt.use_tuple_second" Actionable Dict.empty)
        ]
    }


specialCaseSwapPossibleShouldTupleInCase : Comment -> Rule
specialCaseSwapPossibleShouldTupleInCase =
    Analyzer.functionCalls
        { calledFrom = TopFunction "specialCaseSwapPossible"
        , findExpressions = [ CaseWithPattern Tuple ]
        , find = All
        }


treasureLocationMatchesPlaceLocationUsesPlaceLocationToTreasureLocation : Comment -> Rule
treasureLocationMatchesPlaceLocationUsesPlaceLocationToTreasureLocation =
    Analyzer.functionCalls
        { calledFrom = TopFunction "treasureLocationMatchesPlaceLocation"
        , findExpressions = [ FromSameModule "placeLocationToTreasureLocation" ]
        , find = All
        }


countPlaceTreasuresUsesTupleSecond : Comment -> Rule
countPlaceTreasuresUsesTupleSecond =
    Analyzer.functionCalls
        { calledFrom = TopFunction "countPlaceTreasures"
        , findExpressions = [ FromExternalModule [ "Tuple" ] "second" ]
        , find = All
        }
