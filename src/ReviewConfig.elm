module ReviewConfig exposing (config, ruleConfigs)

import Common.NoUnused
import Common.Simplify
import Exercise.Strain
import Review.Rule as Rule exposing (Rule)
import RuleConfig exposing (RuleConfig)


ruleConfigs : List RuleConfig
ruleConfigs =
    [ -- Common Rules
      Common.NoUnused.ruleConfig
    , Common.Simplify.ruleConfig

    -- Concept Exercises
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
