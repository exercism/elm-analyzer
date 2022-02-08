module Common.NoUnused exposing (config)

-- import NoUnused.Modules

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule

    -- Removed since exercise module is always unused
    --    , NoUnused.Modules.rule
    , NoUnused.Variables.rule
    ]
