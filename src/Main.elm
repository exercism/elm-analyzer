port module Main exposing (main)

import Comment
import Json.Decode as Decode
import ReviewConfig
import RuleConfig


port stdin : (String -> msg) -> Sub msg


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


main : Program () () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , subscriptions = \_ -> stdin identity
        , update = update
        }


update : String -> () -> ( (), Cmd msg )
update input () =
    case input of
        "--extract-comment-paths" ->
            ( ()
            , RuleConfig.getComments ReviewConfig.ruleConfigs
                |> (::) Comment.feedbackComment
                |> List.map Comment.toWebsiteCopyPath
                |> String.join "\n"
                |> stdout
            )

        _ ->
            let
                elmReviewErrorDecoders =
                    RuleConfig.getDecoders ReviewConfig.ruleConfigs
            in
            case Comment.makeSummary elmReviewErrorDecoders input of
                Ok output ->
                    ( (), stdout output )

                Err err ->
                    ( ()
                    , [ "Input could not be parsed. \nError:"
                      , Decode.errorToString err
                      , "Input:"
                      , input
                      ]
                        |> String.join "\n"
                        |> stderr
                    )
