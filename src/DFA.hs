{- |
Module      :  DFA
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Tomasz Homoncik, 2020
License     :  MIT

Maintainer  :  tjhomoncik@gmail.com
Stability   :  experimental
Portability :  portable

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module DFA
  ( DFA(..)
  , neighbors
  , removeUnreachableStates
  ) where

import Set

data DFA state input =
  DFA (Set state) (Set input) (state -> input -> state) state (Set state)
  -- states alphabet (transition function) (initial state) (accepting states)

instance (Show a, Show b) => Show (DFA a b) where
  showsPrec _ (DFA states alphabet transition starting accepting) =
    showList (toList states) .
    showString "\n" .
    showList (toList alphabet) .
    showString "\n" .
    showList
      [(x, y, transition x y) | x <- (toList states), y <- (toList alphabet)] .
    showString "\n" .
    shows starting . showString "\n" . showList (toList accepting)

neighbors :: (Ord state) => (DFA state input) -> state -> Set state
neighbors (DFA _ alphabet transition _ _) n = Set.map (transition n) $ alphabet

removeUnreachableStates :: (Ord state) => (DFA state input) -> (DFA state input)
removeUnreachableStates dfa@(DFA states alphabet transition starting accepting) =
  DFA states' alphabet transition starting accepting'
  where
    states' = closure (neighbors dfa) starting
    accepting' = Set.filter (`elem` accepting) states'
