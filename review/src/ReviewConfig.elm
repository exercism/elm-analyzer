module ReviewConfig exposing (config)

import NoDebug.Log
import NoDebug.TodoOrToString
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify
import UseCamelCase


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Variables.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Modules.rule
    , NoUnused.Exports.rule |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoUnused.Dependencies.rule
    , Simplify.rule Simplify.defaults
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , UseCamelCase.rule UseCamelCase.default
    ]
