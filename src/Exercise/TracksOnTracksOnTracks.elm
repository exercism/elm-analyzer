module Exercise.TracksOnTracksOnTracks exposing (addLanguageUsesCons, countLanguagesUsesLength, excitingListUsesCase, reverseListUsesReverse, ruleConfig)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "tracks-on-tracks-on-tracks"
    , restrictToFiles = Just [ "src/TracksOnTracksOnTracks.elm" ]
    , rules =
        [ CustomRule addLanguageUsesCons
            (Comment "addLanguage doesn't use (::)" "elm.tracks-on-tracks-on-tracks.use_cons" Essential Dict.empty)
        , CustomRule countLanguagesUsesLength
            (Comment "countLanguages doesn't use List.length" "elm.tracks-on-tracks-on-tracks.use_length" Essential Dict.empty)
        , CustomRule reverseListUsesReverse
            (Comment "reverseList doesn't use List.reverse" "elm.tracks-on-tracks-on-tracks.use_reverse" Essential Dict.empty)
        , CustomRule excitingListUsesCase
            (Comment "excitingList doesn't use a case expression" "elm.tracks-on-tracks-on-tracks.use_case" Essential Dict.empty)
        ]
    }


addLanguageUsesCons : Comment -> Rule
addLanguageUsesCons =
    Analyzer.functionCalls
        { calledFrom = TopFunction "addLanguage"
        , findFunctions = [ Operator "::" ]
        , find = Some
        }


countLanguagesUsesLength : Comment -> Rule
countLanguagesUsesLength =
    Analyzer.functionCalls
        { calledFrom = TopFunction "countLanguages"
        , findFunctions = [ FromExternalModule [ "List" ] "length" ]
        , find = Some
        }


reverseListUsesReverse : Comment -> Rule
reverseListUsesReverse =
    Analyzer.functionCalls
        { calledFrom = TopFunction "reverseList"
        , findFunctions = [ FromExternalModule [ "List" ] "reverse" ]
        , find = Some
        }


excitingListUsesCase : Comment -> Rule
excitingListUsesCase =
    Analyzer.functionCalls
        { calledFrom = TopFunction "excitingList"
        , findFunctions = [ CaseBlock ]
        , find = Some
        }
