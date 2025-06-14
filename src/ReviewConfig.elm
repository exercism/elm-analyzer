module ReviewConfig exposing (config, ruleConfigs)

import Common.NoDebug
import Common.NoUnused
import Common.Simplify
import Common.UseCamelCase
import Exercise.AnnalynsInfiltration
import Exercise.Bandwagoner
import Exercise.BettysBikeShop
import Exercise.BlorkemonCards
import Exercise.CustomSet
import Exercise.GithupApi
import Exercise.GottaSnatchEmAll
import Exercise.ListOps
import Exercise.LuciansLusciousLasagna
import Exercise.MariosMarvellousLasagna
import Exercise.MazeMaker
import Exercise.RolePlayingGame
import Exercise.Sieve
import Exercise.Strain
import Exercise.TicketPlease
import Exercise.TisburyTreasureHunt
import Exercise.TopScorers
import Exercise.TracksOnTracksOnTracks
import Exercise.TreasureFactory
import Exercise.ValentinesDay
import Exercise.ZebraPuzzle
import Review.Rule as Rule exposing (Rule)
import RuleConfig exposing (RuleConfig)
import Tags


ruleConfigs : List RuleConfig
ruleConfigs =
    [ -- Tags
      Tags.ruleConfig

    -- Common Rules
    , Common.NoUnused.ruleConfig
    , Common.Simplify.ruleConfig
    , Common.NoDebug.ruleConfig
    , Common.UseCamelCase.ruleConfig

    -- Concept Exercises
    , Exercise.BettysBikeShop.ruleConfig
    , Exercise.RolePlayingGame.ruleConfig
    , Exercise.TopScorers.ruleConfig
    , Exercise.MariosMarvellousLasagna.ruleConfig
    , Exercise.BlorkemonCards.ruleConfig
    , Exercise.TracksOnTracksOnTracks.ruleConfig
    , Exercise.MazeMaker.ruleConfig
    , Exercise.TreasureFactory.ruleConfig
    , Exercise.ValentinesDay.ruleConfig
    , Exercise.Bandwagoner.ruleConfig
    , Exercise.TicketPlease.ruleConfig
    , Exercise.TisburyTreasureHunt.ruleConfig
    , Exercise.GottaSnatchEmAll.ruleConfig
    , Exercise.AnnalynsInfiltration.ruleConfig
    , Exercise.LuciansLusciousLasagna.ruleConfig
    , Exercise.GithupApi.ruleConfig

    -- Practice Exercises
    , Exercise.Strain.ruleConfig
    , Exercise.CustomSet.ruleConfig
    , Exercise.ListOps.ruleConfig
    , Exercise.ZebraPuzzle.ruleConfig
    , Exercise.Sieve.ruleConfig
    ]


config : List Rule
config =
    RuleConfig.makeConfig ruleConfigs
        |> List.map
            (Rule.ignoreErrorsForDirectories [ "tests/" ]
                -- elm.json is standardized
                >> Rule.ignoreErrorsForFiles [ "elm.json" ]
            )
