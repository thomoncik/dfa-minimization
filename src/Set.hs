-- implementation based on BST
module Set
  ( Set(..)
  , closure
  , insert
  , intersection
  , union
  , fromList
  , empty
  , toList
  , Set.filter
  , powerset
  , Set.map
  ) where

data Set a
  = Empty
  | Node (Set a) a (Set a)

instance Eq a => Eq (Set a) where
  (==) a b = (==) (toList a) (toList b)

instance Show a => Show (Set a) where
  show = show . toList

instance Ord a => Ord (Set a) where
  compare a b = compare (toList a) (toList b)

instance Foldable Set where
  foldMap f Empty = mempty
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

insert :: (Ord a) => a -> Set a -> Set a
insert value Empty = Node Empty value Empty
insert value (Node left root right) =
  case compare value root of
    EQ -> Node left value right
    LT -> Node (insert value left) root right
    GT -> Node left root (insert value right)

union :: (Ord a) => Set a -> Set a -> Set a
union tree Empty = tree
union Empty tree = tree
union tree (Node left root right) = union (union (insert root tree) left) right

intersection :: Ord a => Set a -> Set a -> Set a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection a b = fromList $ [aa | aa <- toList a, bb <- toList b, aa == bb]

empty :: Set a
empty = Empty

fromList :: (Ord a) => [a] -> Set a
fromList [] = Empty
fromList (x:xs) = insert x $ fromList xs

toList :: Set a -> [a]
toList Empty = []
toList (Node left root right) = (toList left) ++ [root] ++ (toList right)

filter :: (Ord a) => (a -> Bool) -> Set a -> Set a
filter f = fromList . Prelude.filter f . toList

map :: (Ord b) => (a -> b) -> Set a -> Set b
map f = fromList . Prelude.map f . toList

powerset :: (Ord a) => (Set a) -> (Set (Set a))
powerset a = fromList . Prelude.map fromList $ (f . toList $ a)
  where
    f [] = [[]]
    f (x:xs) = [x : ps | ps <- f xs] ++ f xs

closure :: (Ord a, Eq a) => (a -> (Set a)) -> a -> (Set a)
closure nexts init = go (Set.fromList [init])
  where
    go xs =
      let xs' = foldr Set.union xs (Set.map nexts xs)
       in if xs == xs'
            then xs
            else go xs'
