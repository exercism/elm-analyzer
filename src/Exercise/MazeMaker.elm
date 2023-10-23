module Exercise.MazeMaker exposing (ruleConfig)

import Analyzer exposing (CalledFrom(..), CalledFunction(..), Find(..))
import Comment exposing (Comment, CommentType(..))
import Dict
import Review.Rule exposing (Rule)
import RuleConfig exposing (AnalyzerRule(..), RuleConfig)


ruleConfig : RuleConfig
ruleConfig =
    { slug = Just "maze-maker"
    , restrictToFiles = Just [ "src/MazeMaker.elm" ]
    , rules =
        [ CustomRule roomUsesTreasure
            (Comment "room doesn't use treasure" "elm.maze-maker.room_use_treasure" Essential Dict.empty)
        , CustomRule mazeUsesDeadendRoomAndBranch
            (Comment "maze doesn't use deadend, room and branch" "elm.maze-maker.maze_use_deadend_room_branch" Essential Dict.empty)
        , CustomRule mazeUsesMaze
            (Comment "maze doesn't use itself recursively" "elm.maze-maker.use_maze_recursively" Essential Dict.empty)
        , CustomRule mazeOfDepthUsesDeadendRoomAndBranch
            (Comment "mazeOfDepth doesn't use deadend, room and branch" "elm.maze-maker.mazeOfDepth_use_deadend_room_branch" Essential Dict.empty)
        , CustomRule mazeOfDepthUsesMazeOfDepth
            (Comment "mazeOfDepth doesn't use itself recursively" "elm.maze-maker.use_mazeOfDepth_recursively" Essential Dict.empty)
        ]
    }


roomUsesTreasure : Comment -> Rule
roomUsesTreasure =
    Analyzer.functionCalls
        { calledFrom = TopFunction "room"
        , findFunctions = [ FromSameModule "treasure" ]
        , find = Some
        }


mazeUsesDeadendRoomAndBranch : Comment -> Rule
mazeUsesDeadendRoomAndBranch =
    Analyzer.functionCalls
        { calledFrom = TopFunction "maze"
        , findFunctions = [ FromSameModule "deadend", FromSameModule "room", FromSameModule "branch" ]
        , find = All
        }


mazeUsesMaze : Comment -> Rule
mazeUsesMaze =
    Analyzer.functionCalls
        { calledFrom = TopFunction "maze"
        , findFunctions = [ FromSameModule "maze" ]
        , find = Some
        }


mazeOfDepthUsesDeadendRoomAndBranch : Comment -> Rule
mazeOfDepthUsesDeadendRoomAndBranch =
    Analyzer.functionCalls
        { calledFrom = TopFunction "mazeOfDepth"
        , findFunctions = [ FromSameModule "deadend", FromSameModule "room", FromSameModule "branch" ]
        , find = All
        }


mazeOfDepthUsesMazeOfDepth : Comment -> Rule
mazeOfDepthUsesMazeOfDepth =
    Analyzer.functionCalls
        { calledFrom = TopFunction "mazeOfDepth"
        , findFunctions = [ FromSameModule "mazeOfDepth" ]
        , find = Some
        }
