module TestHelper exposing (..)

import Comment exposing (Comment)
import Expect exposing (Expectation)
import Json.Encode as Encode
import Review.Rule exposing (Rule)
import Review.Test exposing (ExpectedError)


createExpectedErrorUnder : Comment -> String -> ExpectedError
createExpectedErrorUnder comment under =
    Review.Test.error
        { message = Encode.encode 0 (Comment.encodeComment comment)
        , details = [ "" ]
        , under = under
        }


createExpectedGlobalError : Comment -> { message : String, details : List String }
createExpectedGlobalError comment =
    { message = Encode.encode 0 (Comment.encodeComment comment)
    , details = [ "" ]
    }


expectNoErrorsForRules : List Rule -> String -> Expectation
expectNoErrorsForRules rules source =
    Expect.all
        (List.map Review.Test.run rules
            |> List.map (\sourceToResult -> sourceToResult >> Review.Test.expectNoErrors)
        )
        source
