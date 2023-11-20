module Exercise.TracksOnTracksOnTracksTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.TracksOnTracksOnTracks as TracksOnTracksOnTracks
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "TracksOnTracksOnTracksTest"
        [ exemplar
        , otherSolutions
        , noCons
        , noLength
        , noReverse
        , noCase
        ]


rules : List Rule
rules =
    TracksOnTracksOnTracks.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the exemplar" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module TracksOnTracksOnTracks exposing (..)

import List

newList : List a
newList =
    []

existingList : List String
existingList =
    [ "Elm", "Clojure", "Haskell" ]

addLanguage : String -> List String -> List String
addLanguage language languages =
    language :: languages

countLanguages : List a -> Int
countLanguages languages =
    List.length languages

reverseList : List String -> List String
reverseList languages =
    List.reverse languages

excitingList : List String -> Bool
excitingList languages =
    case languages of
        "Elm" :: _ ->
            True

        [ _, "Elm" ] ->
            True

        [ _, "Elm", _ ] ->
            True

        _ ->
            False
"""


otherSolutions : Test
otherSolutions =
    describe "other solutions that are also valid" <|
        [ test "point free style" <|
            \() ->
                TestHelper.expectNoErrorsForRules rules
                    """
module TracksOnTracksOnTracks exposing (..)

newList : List String
newList =
    []

existingList : List String
existingList =
    [ "Elm", "Clojure", "Haskell" ]

addLanguage : String -> List String -> List String
addLanguage =
    (::)

countLanguages : List String -> Int
countLanguages =
    List.length

reverseList : List String -> List String
reverseList =
    List.reverse

excitingList : List String -> Bool
excitingList languages =
    case languages of
        "Elm" :: _ -> True
        _ :: "Elm" :: [] -> True
        _ :: "Elm" :: _ :: [] -> True
        _ -> False
"""
        ]


noCons : Test
noCons =
    let
        comment =
            Comment "elm.tracks-on-tracks-on-tracks.use_cons" Essential Dict.empty
    in
    test "addLanguage doesn't use (::)" <|
        \() ->
            """
module TracksOnTracksOnTracks exposing (..)

addLanguage language languages =
    [language] ++ languages
"""
                |> Review.Test.run (TracksOnTracksOnTracks.addLanguageUsesCons comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "addLanguage" ]


noLength : Test
noLength =
    let
        comment =
            Comment "elm.tracks-on-tracks-on-tracks.use_length" Essential Dict.empty
    in
    test "countLanguages doesn't use List.length" <|
        \() ->
            """
module TracksOnTracksOnTracks exposing (..)

countLanguages list = case list of
  [] -> 0
  _ :: tail -> 1 + countLanguages tail

"""
                |> Review.Test.run (TracksOnTracksOnTracks.countLanguagesUsesLength comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "countLanguages"
                        |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } }
                    ]


noReverse : Test
noReverse =
    let
        comment =
            Comment "elm.tracks-on-tracks-on-tracks.use_reverse" Essential Dict.empty
    in
    test "reverseList doesn't use List.reverse" <|
        \() ->
            """
module TracksOnTracksOnTracks exposing (..)

reverseList list =
  case list of
    [] -> []
    head :: tail -> reverseList tail ++ [head]
"""
                |> Review.Test.run (TracksOnTracksOnTracks.reverseListUsesReverse comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "reverseList"
                        |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                    ]


noCase : Test
noCase =
    let
        comment =
            Comment "elm.tracks-on-tracks-on-tracks.use_case" Essential Dict.empty
    in
    test "excitingList doesn't use a case expression" <|
        \() ->
            """
module TracksOnTracksOnTracks exposing (..)

excitingList languages = 
  if List.head languages == Just "Elm" then
    True
  else if List.head (List.drop 1 languages) == Just "Elm" then
    List.length languages <= 3
  else
    False
"""
                |> Review.Test.run (TracksOnTracksOnTracks.excitingListUsesCase comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder comment "excitingList"
                    ]
