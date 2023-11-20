module Exercise.CustomSetTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.CustomSet as CustomSet
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "CustomSetTest"
        [ exampleSolution
        , usingSet
        ]


rules : List Rule
rules =
    CustomSet.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exampleSolution : Test
exampleSolution =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module CustomSet exposing (diff, disjoint, empty, equal, fromList, insert, intersect, isEmpty, member, subset, toList, union)


type Set
    = Nil
    | Node Int Set Set


empty : Set
empty =
    Nil


insert : Int -> Set -> Set
insert element set =
    case set of
        Nil ->
            Node element Nil Nil

        Node x left right ->
            case compare element x of
                EQ ->
                    set

                LT ->
                    Node x (insert element left) right

                GT ->
                    Node x left (insert element right)


toList : Set -> List Int
toList set =
    case set of
        Nil ->
            []

        Node x left right ->
            toList left ++ x :: toList right


fromList : List Int -> Set
fromList =
    List.foldl insert Nil


isEmpty : Set -> Bool
isEmpty set =
    case set of
        Nil ->
            True

        _ ->
            False


member : Int -> Set -> Bool
member element set =
    case set of
        Nil ->
            False

        Node x left right ->
            case compare element x of
                EQ ->
                    True

                LT ->
                    member element left

                GT ->
                    member element right


equal : Set -> Set -> Bool
equal set1 set2 =
    case ( set1, set2 ) of
        ( Nil, Nil ) ->
            True

        ( Node x1 left right, _ ) ->
            let
                ( l, x1Equalsx2, r ) =
                    splitAt x1 set2
            in
            x1Equalsx2 && equal l left && equal r right

        _ ->
            False


union : Set -> Set -> Set
union set1 set2 =
    case ( set1, set2 ) of
        ( Nil, _ ) ->
            set2

        ( _, Nil ) ->
            set1

        ( Node x1 left right, _ ) ->
            let
                ( l, _, r ) =
                    splitAt x1 set2
            in
            Node x1 (union l left) (union r right)


intersect : Set -> Set -> Set
intersect set1 set2 =
    case ( set1, set2 ) of
        ( Nil, _ ) ->
            Nil

        ( _, Nil ) ->
            Nil

        ( Node x1 left right, _ ) ->
            let
                ( l, x1Equalx2, r ) =
                    splitAt x1 set2
            in
            if x1Equalx2 then
                Node x1 (intersect l left) (intersect r right)

            else
                union (intersect l left) (intersect r right)


diff : Set -> Set -> Set
diff set1 set2 =
    case ( set1, set2 ) of
        ( Nil, _ ) ->
            Nil

        ( _, Nil ) ->
            set1

        ( Node x1 left right, _ ) ->
            let
                ( l, x1Equalsx2, r ) =
                    splitAt x1 set2

                diffLeft =
                    diff left l

                diffRight =
                    diff right r
            in
            if x1Equalsx2 then
                union diffLeft diffRight

            else
                Node x1 diffLeft diffRight


subset : Set -> Set -> Bool
subset set1 set2 =
    diff set1 set2
        |> isEmpty


disjoint : Set -> Set -> Bool
disjoint set1 set2 =
    intersect set1 set2
        |> isEmpty


splitAt : Int -> Set -> ( Set, Bool, Set )
splitAt split set =
    case set of
        Nil ->
            ( Nil, False, Nil )

        Node x left right ->
            case ( compare split x, left, right ) of
                ( EQ, _, _ ) ->
                    ( left, True, right )

                ( LT, Nil, _ ) ->
                    ( Nil, False, set )

                ( LT, _, _ ) ->
                    let
                        ( l, isMember, r ) =
                            splitAt split left
                    in
                    ( l, isMember, Node x r right )

                ( GT, _, Nil ) ->
                    ( set, False, Nil )

                ( GT, _, _ ) ->
                    let
                        ( l, isMember, r ) =
                            splitAt split right
                    in
                    ( Node x left l, isMember, r )

"""


usingSet : Test
usingSet =
    let
        comment =
            Comment "elm.custom-set.do_not_use_set" Essential Dict.empty
    in
    describe "solutions that use the Set function" <|
        [ test "using Set directy" <|
            \() ->
                """
module CustomSet exposing (..)
import Set exposing (Set)

empty : Set Int
empty = Set.empty

insert : Int -> Set Int -> Set Int
insert = Set.insert

toList : Set Int -> List Int
toList = Set.toList


fromList : List Int -> Set Int
fromList = Set.fromList
"""
                    |> Review.Test.run (CustomSet.doNotUseSetModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "Set.empty" ]
        , test "using Set with an alias" <|
            \() ->
                """
module CustomSet exposing (..)
import Set as TotallyNotSet exposing (Set)

empty : Set Int
empty = TotallyNotSet.empty

insert : Int -> Set Int -> Set Int
insert = TotallyNotSet.insert

toList : Set Int -> List Int
toList = TotallyNotSet.toList


fromList : List Int -> Set Int
fromList = TotallyNotSet.fromList
"""
                    |> Review.Test.run (CustomSet.doNotUseSetModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "TotallyNotSet.empty" ]
        , test "using Set with imported functions" <|
            \() ->
                """
module CustomSet exposing (..)
import Set exposing (intersect, isEmpty)

disjoint : Set Int -> Set Int -> Bool
disjoint set1 set2 =
    intersect set1 set2
        |> isEmpty
"""
                    |> Review.Test.run (CustomSet.doNotUseSetModule comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder comment "isEmpty"
                            |> Review.Test.atExactly { start = { row = 8, column = 12 }, end = { row = 8, column = 19 } }
                        ]
        ]
