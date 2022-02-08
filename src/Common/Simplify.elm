module Common.Simplify exposing (config)

import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ Simplify.rule Simplify.defaults
    ]
