module CodeSearch.Query.Trigram
  ( build
  ) where

import           CodeSearch.Regex.Expr
import           CodeSearch.Query.Shrink
import           CodeSearch.Query

import           Control.Applicative
import           Data.Foldable       (foldMap)
import           Data.Semigroup      (option)
import           Data.Set            (Set, toList)

trigrams :: Set String -> TrigramQuery
trigrams = shrinkTrigram . option Any id . foldMap (pure . trigrams') . toList
  where
    trigrams' :: String -> TrigramQuery
    trigrams' []         = Any
    trigrams' [_]        = Any
    trigrams' [_,_]      = Any
    trigrams' [a,b,c]    = Val [a,b,c]
    trigrams' (a:b:c:xs) = Val [a,b,c] `And` trigrams' (b:c:xs)

data Transform = Prefix | Suffix | Exact

-- Information saving transformations
transformWith :: Transform -> RegExpr -> TrigramQuery
transformWith trans = shrinkTrigram . transformWith' trans
  where
    transformWith' Prefix = And <$> match <*> trigrams . prefix
    transformWith' Suffix = And <$> match <*> trigrams . suffix
    transformWith' Exact  = And <$> match <*> maybe Any trigrams . exact

build :: RegExpr -> TrigramQuery
build = transformWith Exact
