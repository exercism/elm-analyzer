module Exercise.MazeMaker exposing (mazeOfDepthUsesDeadendRoomAndBranch, mazeOfDepthUsesMazeOfDepth, mazeUsesDeadendRoomAndBranch, mazeUsesMaze, roomUsesTreasure, ruleConfig)

import Analyzer exposing (CalledExpression(..), CalledFrom(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { restrictToFiles = Just [ "src/MazeMaker.elm" ]
    , rules =
        [ CustomRule roomUsesTreasure
            (Comment "elm.maze-maker.room_use_treasure" Essential Dict.empty)
        , CustomRule mazeUsesDeadendRoomAndBranch
            (Comment "elm.maze-maker.maze_use_deadend_room_branch" Essential Dict.empty)
        , CustomRule mazeUsesMaze
            (Comment "elm.maze-maker.use_maze_recursively" Essential Dict.empty)
        , CustomRule mazeOfDepthUsesDeadendRoomAndBranch
            (Comment "elm.maze-maker.mazeOfDepth_use_deadend_room_branch" Essential Dict.empty)
        , CustomRule mazeOfDepthUsesMazeOfDepth
            (Comment "elm.maze-maker.use_mazeOfDepth_recursively" Essential Dict.empty)
        ]
    }


roomUsesTreasure : Comment -> Rule
roomUsesTreasure =
    Analyzer.functionCalls
        { calledFrom = TopFunction "room"
        , findExpressions = [ FromSameModule "treasure" ]
        , find = Some
        }


mazeUsesDeadendRoomAndBranch : Comment -> Rule
mazeUsesDeadendRoomAndBranch =
    Analyzer.functionCalls
        { calledFrom = TopFunction "maze"
        , findExpressions = [ FromSameModule "deadend", FromSameModule "room", FromSameModule "branch" ]
        , find = All
        }


mazeUsesMaze : Comment -> Rule
mazeUsesMaze =
    Analyzer.functionCalls
        { calledFrom = TopFunction "maze"
        , findExpressions = [ FromSameModule "maze" ]
        , find = Some
        }


mazeOfDepthUsesDeadendRoomAndBranch : Comment -> Rule
mazeOfDepthUsesDeadendRoomAndBranch =
    Analyzer.functionCalls
        { calledFrom = TopFunction "mazeOfDepth"
        , findExpressions = [ FromSameModule "deadend", FromSameModule "room", FromSameModule "branch" ]
        , find = All
        }


mazeOfDepthUsesMazeOfDepth : Comment -> Rule
mazeOfDepthUsesMazeOfDepth =
    Analyzer.functionCalls
        { calledFrom = TopFunction "mazeOfDepth"
        , findExpressions = [ FromSameModule "mazeOfDepth" ]
        , find = Some
        }
