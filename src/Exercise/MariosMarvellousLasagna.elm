module Exercise.MariosMarvellousLasagna exposing (ruleConfig, usesLet)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/MariosMarvellousLasagna.elm" ]
    , rules =
        [ CustomRule usesLet
            (Comment "Doesn't use a let expression" "elm.marios-marvellous-lasagna.use_let" Essential Dict.empty)
        ]
    }


{-| There should be a `let` used in `remainingTimeInMinutes`
-}
usesLet : Comment -> Rule
usesLet =
    Analyzer.functionCalls
        { calledFrom = TopFunction "remainingTimeInMinutes"
        , findExpressions = [ LetBlock ]
        , find = Some
        }
