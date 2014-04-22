module CodeSearch.Util
  ( findFix
  , cartesian
  ) where

import           Data.Monoid
import           Data.Set    (Set)
import qualified Data.Set    as Set

findFix :: Eq a => (a -> a) -> a -> a
findFix f x = let x' = f x in if x == x' then x else findFix f x'

cartesian :: (Ord m, Monoid m) => Set m -> Set m -> Set m
cartesian xs ys = Set.fromList [x `mappend` y | x <- Set.toList xs, y <- Set.toList ys]

