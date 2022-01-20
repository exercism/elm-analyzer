port module Main exposing (main)

import Comment


port stdin : (String -> msg) -> Sub msg


port stdout : String -> Cmd msg


main : Program () () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , subscriptions = \_ -> stdin identity
        , update = \input _ -> ( (), stdout (Comment.makeSummary input) )
        }
