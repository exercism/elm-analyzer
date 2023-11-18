module Common.UseCamelCase exposing (ruleConfig, useCameCaseDecoder)

import Comment exposing (Comment, CommentType(..))
import Dict
import Json.Decode as Decode exposing (Decoder)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)
import UseCamelCase



{-
   Rule source code and tests:
      https://github.com/sparksp/elm-review-camelcase

-}


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Nothing
    , rules =
        [ ImportedRule (UseCamelCase.rule UseCamelCase.default)
            useCameCaseDecoder
            (Comment "UseCamelCase" "elm.common.camelCase" Actionable Dict.empty)
        ]
    }


useCameCaseDecoder : Comment -> Decoder Comment
useCameCaseDecoder comment =
    let
        extractWordBetweenBackticks message =
            case String.split "`" message of
                [ _, word, _ ] ->
                    Decode.succeed word

                _ ->
                    Decode.fail "unexpectedly unable to extract one word between backticks"

        decodeRule =
            Decode.string
                |> Decode.andThen
                    (\rule ->
                        if rule == "UseCamelCase" then
                            Decode.succeed ()

                        else
                            Decode.fail "not UseCamelCase"
                    )

        decodeWrongName =
            Decode.string |> Decode.andThen extractWordBetweenBackticks

        decodeCorrectName =
            Decode.list Decode.string
                |> Decode.map String.concat
                |> Decode.andThen extractWordBetweenBackticks

        toComment : () -> String -> String -> Comment
        toComment _ wrong correct =
            { comment | params = Dict.fromList [ ( "wrong", wrong ), ( "correct", correct ) ] }
    in
    Decode.map3 toComment
        (Decode.field "rule" decodeRule)
        (Decode.field "message" decodeWrongName)
        (Decode.field "details" decodeCorrectName)
