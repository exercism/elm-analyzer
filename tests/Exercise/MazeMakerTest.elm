module Exercise.MazeMakerTest exposing (tests)

import Comment exposing (Comment, CommentType(..))
import Dict
import Exercise.MazeMaker as MazeMaker
import Review.Rule exposing (Rule)
import Review.Test
import RuleConfig
import Test exposing (Test, describe, test)
import TestHelper


tests : Test
tests =
    describe "MazeMakerTest"
        [ exemplar
        , roomUsesTreasure
        , mazeUsesDeadendRoomAndBranch
        , mazeUsesMaze
        , mazeOfDepthUsesDeadendRoomAndBranch
        , mazeOfDepthUsesMazeOfDepth
        ]


rules : List Rule
rules =
    MazeMaker.ruleConfig |> .rules |> List.map RuleConfig.analyzerRuleToRule


exemplar : Test
exemplar =
    test "should not report anything for the example solution" <|
        \() ->
            TestHelper.expectNoErrorsForRules rules
                """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

maze : Generator Maze
maze =
    Random.weighted
        ( 0.6, deadend )
        [ ( 0.15, room )
        , ( 0.25, branch (Random.lazy (\\_ -> maze)) )
        ]
        |> Random.andThen identity

mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    if depth == 0 then
        Random.uniform deadend [ room ] |> Random.andThen identity

    else
        branch (Random.lazy (\\_ -> mazeOfDepth (depth - 1)))
"""


roomUsesTreasure : Test
roomUsesTreasure =
    let
        comment =
            Comment "room doesn't use treasure" "elm.maze-maker.room_use_treasure" Essential Dict.empty
    in
    test "writing room without treasure" <|
        \() ->
            """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room (Random.uniform Friendship [ Gold, Diamond ])
"""
                |> Review.Test.run (MazeMaker.roomUsesTreasure comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder
                        comment
                        "room"
                        |> Review.Test.atExactly { start = { row = 20, column = 1 }, end = { row = 20, column = 5 } }
                    ]


mazeUsesDeadendRoomAndBranch : Test
mazeUsesDeadendRoomAndBranch =
    let
        comment =
            Comment "maze doesn't use deadend, room and branch" "elm.maze-maker.maze_use_deadend_room_branch" Essential Dict.empty
    in
    describe "writing room without deadend, room or branch"
        [ test "without deadend" <|
            \() ->
                """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

maze : Generator Maze
maze =
    Random.weighted
        ( 0.6, Random.constant DeadEnd )
        [ ( 0.15, room )
        , ( 0.25, branch (Random.lazy (\\_ -> maze)) )
        ]
        |> Random.andThen identity
"""
                    |> Review.Test.run (MazeMaker.mazeUsesDeadendRoomAndBranch comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            comment
                            "maze"
                            |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 5 } }
                        ]
        , test "without room" <|
            \() ->
                """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

maze : Generator Maze
maze =
    Random.weighted
        ( 0.6, deadend )
        [ ( 0.15, Random.map Room treasure )
        , ( 0.25, branch (Random.lazy (\\_ -> maze)) )
        ]
        |> Random.andThen identity
"""
                    |> Review.Test.run (MazeMaker.mazeUsesDeadendRoomAndBranch comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            comment
                            "maze"
                            |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 5 } }
                        ]
        , test "without branch" <|
            \() ->
                """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

maze : Generator Maze
maze =
    Random.weighted
        ( 0.6, Random.constant DeadEnd )
        [ ( 0.15, room )
        , ( 0.25
          , Random.int 2 4
                |> Random.andThen
                    (\\n -> Random.list n maze |> Random.map Branch)
          )
        ]
        |> Random.andThen identity
"""
                    |> Review.Test.run (MazeMaker.mazeUsesDeadendRoomAndBranch comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            comment
                            "maze"
                            |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 5 } }
                        ]
        ]


mazeUsesMaze : Test
mazeUsesMaze =
    let
        comment =
            Comment "maze doesn't use itself recursively" "elm.maze-maker.use_maze_recursively" Essential Dict.empty
    in
    test "writing maze without recursion" <|
        \() ->
            """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

maze : Generator Maze
maze =
    Random.weighted
        ( 0.6, deadend )
        [ ( 0.15, room )
        , ( 0.25
          , branch
                (Random.weighted
                    ( 0.6, deadend )
                    [ ( 0.15, room )
                    , ( 0.25
                      , branch
                            (Random.weighted
                                ( 0.6, deadend )
                                [ ( 0.15, room )
                                , ( 0.25, branch room )
                                ]
                                |> Random.andThen identity
                            )
                      )
                    ]
                    |> Random.andThen identity
                )
          )
        ]
        |> Random.andThen identity
"""
                |> Review.Test.run (MazeMaker.mazeUsesMaze comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder
                        comment
                        "maze"
                        |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 5 } }
                    ]


mazeOfDepthUsesDeadendRoomAndBranch : Test
mazeOfDepthUsesDeadendRoomAndBranch =
    let
        comment =
            Comment "mazeOfDepth doesn't use deadend, room and branch" "elm.maze-maker.maze_use_deadend_room_branch" Essential Dict.empty
    in
    describe "writing mazeOfDepth without deadend, room or branch"
        [ test "without deadend" <|
            \() ->
                """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    if depth == 0 then
        Random.uniform (Random.constant DeadEnd) [ room ] |> Random.andThen identity

    else
        branch (Random.lazy (\\_ -> mazeOfDepth (depth - 1)))

"""
                    |> Review.Test.run (MazeMaker.mazeOfDepthUsesDeadendRoomAndBranch comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            comment
                            "mazeOfDepth"
                            |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 12 } }
                        ]
        , test "without room" <|
            \() ->
                """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    if depth == 0 then
        Random.uniform deadend [ Random.map Room treasure ] |> Random.andThen identity

    else
        branch (Random.lazy (\\_ -> mazeOfDepth (depth - 1)))
"""
                    |> Review.Test.run (MazeMaker.mazeOfDepthUsesDeadendRoomAndBranch comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            comment
                            "mazeOfDepth"
                            |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 12 } }
                        ]
        , test "without branch" <|
            \() ->
                """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    if depth == 0 then
        Random.uniform deadend [ room ] |> Random.andThen identity

    else
        Random.int 2 4
            |> Random.andThen
                (\\n -> Random.list n (mazeOfDepth (depth - 1)) |> Random.map Branch)
"""
                    |> Review.Test.run (MazeMaker.mazeOfDepthUsesDeadendRoomAndBranch comment)
                    |> Review.Test.expectErrors
                        [ TestHelper.createExpectedErrorUnder
                            comment
                            "mazeOfDepth"
                            |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 12 } }
                        ]
        ]


mazeOfDepthUsesMazeOfDepth : Test
mazeOfDepthUsesMazeOfDepth =
    let
        comment =
            Comment "mazeOfDepth doesn't use itself recursively" "elm.maze-maker.use_mazeOfDepth_recursively" Essential Dict.empty
    in
    test "writing mazeOfDepth without recursion" <|
        \() ->
            """
module MazeMaker exposing (..)
import Random exposing (Generator)

type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)

type Treasure
    = Gold
    | Diamond
    | Friendship

deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Friendship [ Gold, Diamond ]

room : Generator Maze
room =
    Random.map Room treasure

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen
            (\\n -> Random.list n mazeGenerator |> Random.map Branch)

mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    let
        bottom =
            Random.uniform deadend [ room ] |> Random.andThen identity
    in
    List.foldl (\\_ gen -> branch gen) bottom (List.range 1 depth)
"""
                |> Review.Test.run (MazeMaker.mazeOfDepthUsesMazeOfDepth comment)
                |> Review.Test.expectErrors
                    [ TestHelper.createExpectedErrorUnder
                        comment
                        "mazeOfDepth"
                        |> Review.Test.atExactly { start = { row = 34, column = 1 }, end = { row = 34, column = 12 } }
                    ]
