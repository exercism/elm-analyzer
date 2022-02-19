module Exercise.Strain exposing (doNotUseFilter, ruleConfig)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "strain"
    , restrictToFiles = Just [ "src/Strain.elm" ]
    , rules = [ doNotUseFilter ]
    , highjackErrorDecoders = []
    }


doNotUseFilter : Rule
doNotUseFilter =
    Analyzer.functionCalls
        { calledFrom = Anywhere
        , findFunctions = [ FromExternalModule [ "List" ] "filter", FromExternalModule [ "List" ] "filterMap" ]
        , find = None
        , comment = Comment "Uses the List module" "elm.strain.do_not_use_filter" Essential Dict.empty
        }
