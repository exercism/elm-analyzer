module Exercise.GottaSnatchEmAll exposing (boringCardsUsesFold, boringCardsUsesIntersect, extraCardsUsesDiff, removeDuplicatesUsesSet, ruleConfig, splitShinyCardsUsesPartition, totalCardsUsesFold, totalCardsUsesUnion, usesSingleton)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/GottaSnatchEmAll.elm" ]
    , rules =
        [ CustomRule usesSingleton
            (Comment "elm.gotta-snatch-em-all.use_singleton" Actionable Dict.empty)
        , CustomRule removeDuplicatesUsesSet
            (Comment "elm.gotta-snatch-em-all.use_set" Essential Dict.empty)
        , CustomRule extraCardsUsesDiff
            (Comment "elm.gotta-snatch-em-all.use_diff" Essential Dict.empty)
        , CustomRule boringCardsUsesIntersect
            (Comment "elm.gotta-snatch-em-all.use_intersect" Essential Dict.empty)
        , CustomRule boringCardsUsesFold
            (Comment "elm.gotta-snatch-em-all.boringCards_use_fold" Actionable Dict.empty)
        , CustomRule totalCardsUsesUnion
            (Comment "elm.gotta-snatch-em-all.use_union" Essential Dict.empty)
        , CustomRule totalCardsUsesFold
            (Comment "elm.gotta-snatch-em-all.totalCards_use_fold" Actionable Dict.empty)
        , CustomRule splitShinyCardsUsesPartition
            (Comment "elm.gotta-snatch-em-all.use_partition" Essential Dict.empty)
        ]
    }


usesSingleton : Comment -> Rule
usesSingleton =
    Analyzer.functionCalls
        { calledFrom = TopFunction "newCollection"
        , findExpressions = [ FromExternalModule [ "Set" ] "singleton" ]
        , find = Some
        }


removeDuplicatesUsesSet : Comment -> Rule
removeDuplicatesUsesSet =
    Analyzer.functionCalls
        { calledFrom = TopFunction "removeDuplicates"
        , findExpressions = [ AnyFromExternalModule [ "Set" ] ]
        , find = Some
        }


extraCardsUsesDiff : Comment -> Rule
extraCardsUsesDiff =
    Analyzer.functionCalls
        { calledFrom = TopFunction "extraCards"
        , findExpressions = [ FromExternalModule [ "Set" ] "diff" ]
        , find = Some
        }


boringCardsUsesIntersect : Comment -> Rule
boringCardsUsesIntersect =
    Analyzer.functionCalls
        { calledFrom = TopFunction "boringCards"
        , findExpressions = [ FromExternalModule [ "Set" ] "intersect" ]
        , find = Some
        }


boringCardsUsesFold : Comment -> Rule
boringCardsUsesFold =
    Analyzer.functionCalls
        { calledFrom = TopFunction "boringCards"
        , findExpressions = [ FromExternalModule [ "List" ] "foldl", FromExternalModule [ "List" ] "foldr" ]
        , find = Some
        }


totalCardsUsesUnion : Comment -> Rule
totalCardsUsesUnion =
    Analyzer.functionCalls
        { calledFrom = TopFunction "totalCards"
        , findExpressions = [ FromExternalModule [ "Set" ] "union" ]
        , find = Some
        }


totalCardsUsesFold : Comment -> Rule
totalCardsUsesFold =
    Analyzer.functionCalls
        { calledFrom = TopFunction "totalCards"
        , findExpressions = [ FromExternalModule [ "List" ] "foldl", FromExternalModule [ "List" ] "foldr" ]
        , find = Some
        }


splitShinyCardsUsesPartition : Comment -> Rule
splitShinyCardsUsesPartition =
    Analyzer.functionCalls
        { calledFrom = TopFunction "splitShinyCards"
        , findExpressions = [ FromExternalModule [ "Set" ] "partition" ]
        , find = Some
        }
