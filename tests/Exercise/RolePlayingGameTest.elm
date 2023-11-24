module Exercise.RolePlayingGameTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.RolePlayingGame as RolePlayingGame
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "RolePlayingGameTest"
        [ exemplar
        , doesNotUseWithDefault
        ]


rules : List Rule
rules =
    RolePlayingGame.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module RolePlayingGame exposing (Player, castSpell, introduce, revive)

type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }

introduce : Player -> String
introduce { name } =
    Maybe.withDefault "Mighty Magician" name

revive : Player -> Maybe Player
revive player =
    if player.health > 0 then
        Nothing

    else if player.level >= 10 then
        Just { player | health = 100, mana = Just 100 }

    else
        Just { player | health = 100, mana = Nothing }

castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Nothing ->
            ( { player | health = max 0 (player.health - manaCost) }, 0 )

        Just mana ->
            if mana >= manaCost then
                ( { player | mana = Just (mana - manaCost) }, 2 * manaCost )

            else
                ( player, 0 )
"""


doesNotUseWithDefault : Test
doesNotUseWithDefault =
    let
        comment =
            Comment "elm.role-playing-game.use_withDefault" Actionable Dict.empty
    in
    test "introduce doesn't use Maybe.withDefault" <|
        \() ->
            """
module RolePlayingGame exposing (Player, castSpell, introduce, revive)

type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }

introduce : Player -> String
introduce { name } =
    case name of
        Nothing -> "Mighty Magician"
        Just n -> n
"""
                |> Review.Test.run (RolePlayingGame.useWithDefault comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "introduce"
                        |> Review.Test.atExactly { start = { row = 12, column = 1 }, end = { row = 12, column = 10 } }
                    ]
