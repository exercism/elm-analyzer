module RuleConfig exposing (RuleConfig, makeConfig)

import Comment exposing (Comment)
import Json.Decode exposing (Decoder)
import Review.Rule as Rule exposing (Rule)


type alias RuleConfig =
    { slug : Maybe String
    , restrictToFiles : Maybe (List String)
    , rules : List Rule
    , highjackErrorDecoders : List (Decoder Comment)
    }


getRules : RuleConfig -> List Rule
getRules { rules, restrictToFiles } =
    case restrictToFiles of
        Nothing ->
            rules

        Just files ->
            List.map (Rule.filterErrorsForFiles (\file -> List.member file files)) rules


makeConfig : List RuleConfig -> List Rule
makeConfig ruleConfigs =
    ruleConfigs
        |> List.map getRules
        |> List.concat
