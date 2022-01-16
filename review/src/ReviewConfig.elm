module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Common.NoUnused
import Common.Simplify
import Exercise.TwoFer
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ Common.NoUnused.config
    , Common.Simplify.config
    , Exercise.TwoFer.config
    ]
        |> List.concat
        |> List.map
            (Rule.ignoreErrorsForDirectories [ "tests/" ]
                >> Rule.ignoreErrorsForFiles [ "elm.json" ]
            )
