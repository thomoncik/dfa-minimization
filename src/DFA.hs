module DFA
  ( DFA(..)
  , neighbors
  , removeUnreachableStates
  ) where

import Set

data DFA state input =
  DFA (Set state) (Set input) (state -> input -> state) state (Set state)

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
