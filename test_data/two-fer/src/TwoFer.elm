module TwoFer exposing (twoFer)

import Maybe exposing (withDefault)
import Maybe as Definitely

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

twoFerWithDefaultImported: Maybe String -> String
twoFerWithDefaultImported name =     "One for "
        ++ withDefault "you" name
        ++ ", one for me."

twoFerWithDefaultAliased: Maybe String -> String
twoFerWithDefaultAliased name =     "One for "
        ++ Definitely.withDefault   "you" name
        ++ ", one for me." 