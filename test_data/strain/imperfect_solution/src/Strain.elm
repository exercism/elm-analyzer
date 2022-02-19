module Strain exposing (discard, keep)

import List exposing (filter)


keep : (a -> Bool) -> List a -> List a
keep predicate =
    filter predicate


discard : (a -> Bool) -> List a -> List a
discard predicate =
    filter (predicate >> not)
