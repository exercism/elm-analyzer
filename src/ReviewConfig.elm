module ReviewConfig exposing (config, ruleConfigs)

import Common.NoDebug
import Common.NoUnused
import Common.Simplify
import Common.UseCamelCase
import Exercise.Bandwagoner
import Exercise.BettysBikeShop
import Exercise.BlorkemonCards
import Exercise.CustomSet
import Exercise.ListOps
import Exercise.MariosMarvellousLasagna
import Exercise.MazeMaker
import Exercise.Strain
import Exercise.TicketPlease
import Exercise.TopScorers
import Exercise.TracksOnTracksOnTracks
import Exercise.TreasureFactory
import Exercise.ValentinesDay
import Exercise.ZebraPuzzle
import Review.Rule as Rule exposing (Rule)
import RuleConfig exposing (RuleConfig)


ruleConfigs : List RuleConfig
ruleConfigs =
    [ -- Common Rules
      Common.NoUnused.ruleConfig
    , Common.Simplify.ruleConfig
    , Common.NoDebug.ruleConfig
    , Common.UseCamelCase.ruleConfig

    -- Concept Exercises
    , Exercise.BettysBikeShop.ruleConfig
    , Exercise.TopScorers.ruleConfig
    , Exercise.MariosMarvellousLasagna.ruleConfig
    , Exercise.BlorkemonCards.ruleConfig
    , Exercise.TracksOnTracksOnTracks.ruleConfig
    , Exercise.MazeMaker.ruleConfig
    , Exercise.TreasureFactory.ruleConfig
    , Exercise.ValentinesDay.ruleConfig
    , Exercise.Bandwagoner.ruleConfig
    , Exercise.TicketPlease.ruleConfig

    -- Practice Exercises
    , Exercise.Strain.ruleConfig
    , Exercise.CustomSet.ruleConfig
    , Exercise.ListOps.ruleConfig
    , Exercise.ZebraPuzzle.ruleConfig
    ]


config : List Rule
config =
    RuleConfig.makeConfig ruleConfigs
        |> List.map
            (Rule.ignoreErrorsForDirectories [ "tests/" ]
                -- elm.json is standardized
                >> Rule.ignoreErrorsForFiles [ "elm.json" ]
            )
