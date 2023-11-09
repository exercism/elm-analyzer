module TwoFer exposing (..)

-- unused imported function
import Maybe exposing (withDefault)
-- unused imported module
import Parser.Advanced

-- unused type constructor TwoFerUnused
-- unused type constructor argument String 
type TwoFer = TwoFerUnused String

twoFer : Maybe String -> String
twoFer name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."

twoFerNoSignature name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."

twoFerUsesCase : Maybe String -> String
twoFerUsesCase name =
  case name of
    Nothing -> "One for you, one for me."
    Just you -> "One for " ++ you ++ ", one for me."

unusedParameter : String -> Int
-- unused is unused
unusedParameter unused = 
  case Nothing of
    -- 1 * 1 can be simplified
    -- something is unused
    Just something -> 1 * 1
    -- True && False can be simplified to False
    -- if condition can be simplified to 0
    Nothing -> if (True && False) then 1 else 0

-- function not in camelCase
snake_case_two_fer name =
    "One for "
        ++ Maybe.withDefault "you" name
        ++ ", one for me."
