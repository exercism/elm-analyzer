module Comment exposing (..)

-- import Debug

import Dict exposing (Dict)
import Elm.Syntax.Range as Range
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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


makeSummary : String -> String
makeSummary input =
    case Decode.decodeString (Decode.list decodeComment) input of
        Ok comments ->
            aggregateComments comments |> encodeSummary |> Encode.encode 0

        Err _ ->
            emptySummary |> Encode.encode 0


aggregateComments : List Comment -> Summary
aggregateComments comments =
    let
        sortedComments =
            List.sortBy (.commentType >> commentTypeShowOrder) comments

        message =
            case List.map .commentType comments |> minimumBy commentTypeSummaryOrder of
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


minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f list =
    let
        getMinimum element ( currentMin, currentMinValue ) =
            if f element < currentMinValue then
                ( element, f element )

            else
                ( currentMin, currentMinValue )
    in
    case list of
        [] ->
            Nothing

        head :: tail ->
            List.foldl getMinimum ( head, f head ) tail |> Tuple.first |> Just


emptySummary : Value
emptySummary =
    Encode.object [ ( "comments", Encode.list encodeComment [] ) ]



-- CREATING ERROR FROM COMMENT


createError : Comment -> Error {}
createError comment =
    Rule.error
        { message = Encode.encode 0 (encodeComment comment)
        , details = []
        }
        Range.emptyRange



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
        , ( "type", commentType |> encodeCommentType |> Encode.string )
        , ( "params", Encode.dict identity Encode.string params )
        ]


encodeComment : Comment -> Value
encodeComment { name, comment, commentType, params } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "comment", Encode.string comment )
        , ( "type", commentType |> encodeCommentType |> Encode.string )
        , ( "params", Encode.dict identity Encode.string params )
        ]


decodeComment : Decoder Comment
decodeComment =
    Decode.map4 Comment
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "type" decodeCommentType)
        (Decode.field "params" (Decode.dict Decode.string))


encodeCommentType : CommentType -> String
encodeCommentType commentType =
    case commentType of
        Essential ->
            "essential"

        Actionable ->
            "actionable"

        Informative ->
            "informative"

        Celebratory ->
            "celebratory"


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