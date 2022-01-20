module TestHelper exposing (..)

import Expect exposing (Expectation)
import Review.Rule exposing (Rule)
import Review.Test


expectNoErrorsForRules : List Rule -> String -> Expectation
expectNoErrorsForRules rules source =
    Expect.all
        (List.map Review.Test.run rules
            |> List.map (\sourceToResult -> sourceToResult >> Review.Test.expectNoErrors)
        )
        source
