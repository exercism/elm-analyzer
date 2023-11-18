module Exercise.AnnalynsInfiltration exposing (canFastAttackUsesNot, canFreePrisonerUsesBooleanOperators, canSignalPrisonerUsesBooleanOperators, canSpyUsesOr, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "annalyns-infiltration"
    , restrictToFiles = Just [ "src/AnnalynsInfiltration.elm" ]
    , rules =
        [ CustomRule canFastAttackUsesNot
            (Comment "canFastAttack doesn't use not" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty)
        , CustomRule canSpyUsesOr
            (Comment "canSpy doesn't use ||" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty)
        , CustomRule canSignalPrisonerUsesBooleanOperators
            (Comment "canSignalPrisoner doesn't use boolean operators" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty)
        , CustomRule canFreePrisonerUsesBooleanOperators
            (Comment "canFreePrisoner doesn't use boolean operators" "elm.annalyns-infiltration.use_bool_operators" Essential Dict.empty)
        ]
    }


canFastAttackUsesNot : Comment -> Rule
canFastAttackUsesNot =
    Analyzer.functionCalls
        { calledFrom = TopFunction "canFastAttack"
        , findExpressions = [ FromExternalModule [ "Basics" ] "not" ]
        , find = Some
        }


canSpyUsesOr : Comment -> Rule
canSpyUsesOr =
    Analyzer.functionCalls
        { calledFrom = TopFunction "canSpy"
        , findExpressions = [ Operator "||" ]
        , find = Some
        }


canSignalPrisonerUsesBooleanOperators : Comment -> Rule
canSignalPrisonerUsesBooleanOperators =
    Analyzer.functionCalls
        { calledFrom = TopFunction "canSignalPrisoner"
        , findExpressions = [ Operator "||", Operator "&&" ]
        , find = Some
        }


canFreePrisonerUsesBooleanOperators : Comment -> Rule
canFreePrisonerUsesBooleanOperators =
    Analyzer.functionCalls
        { calledFrom = TopFunction "canFreePrisoner"
        , findExpressions = [ Operator "||", Operator "&&" ]
        , find = Some
        }
