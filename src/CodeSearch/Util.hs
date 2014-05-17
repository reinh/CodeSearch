module CodeSearch.Util
  ( findFix
  , cartesian
  ) where

import           Data.Monoid (Monoid,mappend)
import           Data.Set    (Set,fromList,toList)

findFix :: Eq a => (a -> a) -> a -> a
findFix f x = let x' = f x in if x == x' then x else findFix f x'

cartesian :: (Ord m, Monoid m) => Set m -> Set m -> Set m
cartesian xs ys = fromList [x `mappend` y | x <- toList xs, y <- toList ys]

