{- |
Module      :  NFA
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Tomasz Homoncik, 2020
License     :  MIT

Maintainer  :  tjhomoncik@gmail.com
Stability   :  experimental
Portability :  portable

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module NFA
  ( NFA(..)
  ) where

import Set

data NFA state input =
  NFA
    (Set state)
    (Set input)
    (state -> Maybe input -> (Set state))
    (Set state)
    (Set state)
  -- states alphabet (transition function) (initial states) (accepting states)

instance (Ord a, Ord b, Show a, Show b) => Show (NFA a b) where
  showsPrec _ (NFA states alphabet transition starting accepting) =
    showList (toList states) .
    showString "\n" .
    showList (toList alphabet) .
    showString "\n" .
    showList
      [ (x, y, transition x y)
      | x <- (toList states)
      , y <- Prelude.map Just (toList alphabet) ++ [Nothing]
      ] .
    showString "\n" .
    showList (toList starting) . showString "\n" . showList (toList accepting)

instance (Ord a, Ord b, Read a, Read b) => Read (NFA a b) where
  readsPrec _ input =
    let [l0, l1, l2, l3, l4] = lines input
     in [ ( NFA
              (fromList $ read l0)
              (fromList $ read l1)
              (fromTuples (read l2))
              (fromList $ read l3)
              (fromList $ read l4)
          , "")
        ]

fromTuples :: (Ord a, Eq b) => [(a, Maybe b, a)] -> (a -> Maybe b -> (Set a))
fromTuples tuples a b =
  Set.fromList .
  Prelude.map (\(x, y, z) -> z) .
  Prelude.filter (\(x, y, z) -> (x, y) == (a, b)) $
  tuples
