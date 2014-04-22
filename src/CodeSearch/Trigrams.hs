module CodeSearch.Trigrams
  ( trigrams
  ) where

import CodeSearch.Expr
import CodeSearch.Shrink
import CodeSearch.Types

import           Control.Applicative
import           Data.Foldable       (foldMap)
import           Data.Semigroup      hiding (Any)
import           Data.Set            (Set)
import qualified Data.Set            as Set

trigrams :: Set String -> TrigramExpr String
trigrams = shrinkTrigram . option Any id . foldMap (pure . trigrams') . Set.toList
  where
    trigrams' :: String -> TrigramExpr String
    trigrams' []         = Any
    trigrams' [_]        = Any
    trigrams' [_,_]      = Any
    trigrams' [a,b,c]    = Val [a,b,c]
    trigrams' (a:b:c:xs) = Val [a,b,c] `And` trigrams' (b:c:xs)

-- Information saving transformations
transformWith :: Transform -> Expr Char -> TrigramExpr String
transformWith trans = shrinkTrigram . transformWith' trans
  where
    transformWith' Prefix = And <$> match <*> trigrams . prefix
    transformWith' Suffix = And <$> match <*> trigrams . suffix
    transformWith' Exact  = And <$> match <*> maybe Any trigrams . exact

