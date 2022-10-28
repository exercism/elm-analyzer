module ReviewConfig exposing (config, ruleConfigs)

import Common.NoUnused
import Common.Simplify
import Exercise.BettysBikeShop
import Exercise.BlorkemonCards
import Exercise.MariosMarvellousLasagna
import Exercise.Strain
import Exercise.TopScorers
import Review.Rule as Rule exposing (Rule)
import RuleConfig exposing (RuleConfig)


ruleConfigs : List RuleConfig
ruleConfigs =
    [ -- Common Rules
      Common.NoUnused.ruleConfig
    , Common.Simplify.ruleConfig

    -- Concept Exercises
    , Exercise.BettysBikeShop.ruleConfig
    , Exercise.TopScorers.ruleConfig
    , Exercise.MariosMarvellousLasagna.ruleConfig
    , Exercise.BlorkemonCards.ruleConfig

    -- Practice Exercises
    , Exercise.Strain.ruleConfig
    ]


config : List Rule
config =
    RuleConfig.makeConfig ruleConfigs
        |> List.map
            (Rule.ignoreErrorsForDirectories [ "tests/" ]
                -- elm.json is standardized
                >> Rule.ignoreErrorsForFiles [ "elm.json" ]
            )
