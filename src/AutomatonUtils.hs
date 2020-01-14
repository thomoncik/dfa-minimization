{- |
Module      :  AutomatonUtils
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Tomasz Homoncik, 2020
License     :  MIT

Maintainer  :  tjhomoncik@gmail.com
Stability   :  experimental
Portability :  portable

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module AutomatonUtils
  ( reverse
  , toDfa
  , minimize
  ) where

import DFA
import NFA
import Set

import Prelude hiding (reverse)

toDfa :: (Ord a, Eq b) => NFA a b -> DFA (Set a) b
toDfa (NFA states alphabet transition starting accepting) =
  removeUnreachableStates $
  DFA states' alphabet' transition' starting' accepting'
  where
    states' = powerset states
    alphabet' = alphabet
    transition' from given =
      foldr union empty . Set.map (\s -> transition s (Just given)) $ from
    starting' = starting
    accepting' =
      fromList
        [ state
        | state <- (toList states')
        , state `intersection` accepting /= empty
        ]

reverse :: (Eq a, Ord a) => DFA a b -> NFA a b
reverse (DFA states alphabet delta starting accepting) =
  NFA states alphabet delta' accepting (Set.fromList [starting])
  where
    delta' st (Just sy) =
      Set.fromList [s | s <- (toList states), delta s sy == st]
    delta' st Nothing = empty

minimize :: (Eq a, Eq b, Ord a) => DFA a b -> DFA (Set (Set a)) b
minimize = toDfa . reverse . toDfa . reverse
