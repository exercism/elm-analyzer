module Comment exposing
    ( Comment
    , CommentType(..)
    , Summary
    , aggregateComments
    , createError
    , createGlobalError
    , decodeComment
    , encodeComment
    , makeSummary
    )

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Review.Rule as Rule exposing (Error)



-- TYPES


type alias Summary =
    { summary : String
    , comments : List Comment
    }


type alias Comment =
    { name : String
    , comment : String
    , commentType : CommentType
    , params : Dict String String
    }


type CommentType
    = Celebratory
    | Essential
    | Actionable
    | Informative



-- BUILDING SUMMARY


{-| Transform `elm-review` JSON output into an exercism compliant `analysis.json`.

`elm-review` outputs the errors in a JSON format unsuited for Exercism specifications.
The output is therefore fed into `makeSummary` to be transformed.

`elm-review` rule errors are string-only, so the analyzer custom rules encode `Comment`s into JSON strings.
Those strings are extracted in `decodeElmReviewComments`, and decoded into `Comment`s again.

For using existing rules from the Elm community on every solution, custom decoders must be provided.
Those decoders are the `elmReviewErrorDecoders`, custom-made to transform the `elm-review` raw outputs into `Comment`s.
It is unfortunate that for some of them, the easy solution is to directly pass the `elm-review` output via a parameter.
This should be avoided as much as possible, and removed in the long term.

After gathering all the `Comment`s, they are reordered according to their type, a suitable summary is created and the final JSON is exported.

-}
makeSummary : List (Decoder Comment) -> String -> Result Decode.Error String
makeSummary elmReviewErrorDecoders =
    decodeElmReviewComments elmReviewErrorDecoders
        |> Decode.map (aggregateComments >> encodeSummary >> Encode.encode 0)
        |> Decode.decodeString


aggregateComments : List Comment -> Summary
aggregateComments comments =
    let
        sortedComments =
            List.sortBy (.commentType >> commentTypeShowOrder) comments

        message =
            case List.map .commentType comments |> List.Extra.minimumBy commentTypeSummaryOrder of
                Nothing ->
                    "No suggestions found."

                Just Essential ->
                    -- \u{202F} is a narrow no-break space
                    -- https://en.wikipedia.org/wiki/Non-breaking_space
                    "Check the comments for things to fix.\u{202F}🛠 "

                Just Actionable ->
                    "Check the comments for some suggestions.\u{202F}📣"

                Just Informative ->
                    "Check the comments for some things to learn.\u{202F}📖"

                Just Celebratory ->
                    "You're doing something right.\u{202F}🎉"
    in
    Summary message sortedComments


commentTypeShowOrder : CommentType -> Int
commentTypeShowOrder commentType =
    case commentType of
        Celebratory ->
            0

        Essential ->
            1

        Actionable ->
            2

        Informative ->
            3


commentTypeSummaryOrder : CommentType -> Int
commentTypeSummaryOrder commentType =
    case commentType of
        Essential ->
            0

        Actionable ->
            1

        Informative ->
            2

        Celebratory ->
            3



-- CREATING ERROR FROM COMMENT


createError : Comment -> Range -> Error {}
createError comment range =
    Rule.error
        { message = Encode.encode 0 (encodeComment comment)
        , details = [ "" ]
        }
        range


createGlobalError : Comment -> Error {}
createGlobalError comment =
    Rule.globalError
        { message = Encode.encode 0 (encodeComment comment)
        , details = [ "" ]
        }



--ENCODERS, DECODERS


encodeSummary : Summary -> Value
encodeSummary { summary, comments } =
    Encode.object
        [ ( "summary", Encode.string summary )
        , ( "comments", Encode.list encodeSummaryComment comments )
        ]


encodeSummaryComment : Comment -> Value
encodeSummaryComment { comment, commentType, params } =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "type", commentType |> encodeCommentType )
        , ( "params", Encode.dict identity Encode.string params )
        ]


encodeComment : Comment -> Value
encodeComment { name, comment, commentType, params } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "comment", Encode.string comment )
        , ( "type", commentType |> encodeCommentType )
        , ( "params", Encode.dict identity Encode.string params )
        ]


decodeElmReviewComments : List (Decoder Comment) -> Decoder (List Comment)
decodeElmReviewComments elmReviewErrorDecoders =
    let
        decodeErrorsPerFile =
            Decode.oneOf (decodeMessage :: elmReviewErrorDecoders)
                |> Decode.list
                |> Decode.field "errors"

        decodeMessage =
            Decode.string
                |> Decode.andThen
                    (\message ->
                        case Decode.decodeString decodeComment message of
                            Ok comment ->
                                Decode.succeed comment

                            Err err ->
                                Decode.fail (Decode.errorToString err)
                    )
                |> Decode.field "message"
    in
    Decode.list decodeErrorsPerFile
        |> Decode.map List.concat
        |> Decode.field "errors"


decodeComment : Decoder Comment
decodeComment =
    Decode.map4 Comment
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "type" decodeCommentType)
        (Decode.field "params" (Decode.dict Decode.string))


encodeCommentType : CommentType -> Value
encodeCommentType commentType =
    (case commentType of
        Essential ->
            "essential"

        Actionable ->
            "actionable"

        Informative ->
            "informative"

        Celebratory ->
            "celebratory"
    )
        |> Encode.string


decodeCommentType : Decoder CommentType
decodeCommentType =
    let
        decodeType commentType =
            case commentType of
                "essential" ->
                    Decode.succeed Essential

                "actionable" ->
                    Decode.succeed Actionable

                "informative" ->
                    Decode.succeed Informative

                "celebratory" ->
                    Decode.succeed Celebratory

                other ->
                    Decode.fail ("Invalid comment type " ++ other)
    in
    Decode.string |> Decode.andThen decodeType
