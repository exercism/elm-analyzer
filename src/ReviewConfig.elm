module ReviewConfig exposing (config)

import Exercise.TwoFer
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ -- Common Checks: NoUnused
      NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Variables.rule

    -- Common Check: Simplify
    , Simplify.rule Simplify.defaults

    -- Pratice Exercise: two-fer
    , Exercise.TwoFer.hasFunctionSignature
    , Exercise.TwoFer.usesWithDefault
    ]
        |> List.map
            (Rule.ignoreErrorsForDirectories [ "tests/" ]
                >> Rule.ignoreErrorsForFiles [ "elm.json" ]
            )
