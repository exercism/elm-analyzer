module Exercise.RolePlayingGame exposing (ruleConfig, useWithDefault)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/RolePlayingGame.elm" ]
    , rules =
        [ CustomRule useWithDefault
            (Comment "elm.role-playing-game.use_withDefault" Actionable Dict.empty)
        ]
    }


useWithDefault : Comment -> Rule
useWithDefault =
    Analyzer.functionCalls
        { calledFrom = TopFunction "introduce"
        , findExpressions = [ FromExternalModule [ "Maybe" ] "withDefault" ]
        , find = Some
        }
