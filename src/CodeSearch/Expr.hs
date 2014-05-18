module CodeSearch.Expr
  ( shrinkExpr
  , emptyable
  , exact
  , prefix
  , suffix
  , match
  ) where

import           CodeSearch.Shrink
import           CodeSearch.Types
import           CodeSearch.Util

import           Control.Applicative
import           Data.Set            (Set, singleton, union)

shrinkExpr :: RegExpr -> RegExpr
shrinkExpr (e `Concat` Empty) = e
shrinkExpr (Empty `Concat` e) = e
shrinkExpr (e `Alt` Empty) = e
shrinkExpr (Empty `Alt` e) = e
shrinkExpr e = e

emptyable :: RegExpr -> Bool
emptyable Empty           = True
emptyable (Single     _)  = False
emptyable (ZeroOrOne  _)  = True
emptyable (ZeroOrMore _)  = True
emptyable (OneOrMore  e)  = emptyable e
emptyable (Alt    e1 e2)  = emptyable e1 || emptyable e2
emptyable (Concat e1 e2)  = emptyable e1 && emptyable e2

exact :: RegExpr -> Maybe (Set String)
exact Empty          = Just (singleton "")
exact (Single     c) = Just (singleton [c])
exact (ZeroOrOne  c) = union (singleton "") <$> exact c
exact (ZeroOrMore _) = Nothing
exact (OneOrMore  _) = Nothing
exact (Alt    e1 e2) = union <$> exact e1 <*> exact e2
exact (Concat e1 e2) = cartesian <$> exact e1 <*> exact e2

prefix :: RegExpr -> Set String
prefix Empty          = singleton ""
prefix (Single     c) = singleton [c]
prefix (ZeroOrOne  _) = singleton ""
prefix (ZeroOrMore _) = singleton ""
prefix (OneOrMore  e) = prefix e
prefix (Alt    e1 e2) = prefix e1 `union` prefix e2
prefix (Concat e1 e2) =
  case exact e1 of
    Just s  -> s `cartesian` prefix e2
    Nothing -> if emptyable e1
                  then prefix e1 `union` prefix e2
                  else prefix e1

suffix :: RegExpr -> Set String
suffix Empty          = singleton ""
suffix (Single     c) = singleton [c]
suffix (ZeroOrOne  _) = singleton ""
suffix (ZeroOrMore _) = singleton ""
suffix (OneOrMore  e) = suffix e
suffix (Alt    e1 e2) = suffix e1 `union` suffix e2
suffix (Concat e1 e2) =
  case exact e2 of
    Just s  -> suffix e1 `cartesian` s
    Nothing -> if emptyable e2
                  then suffix e2 `union` suffix e1
                  else suffix e2

match :: RegExpr -> TrigramQuery
match = shrinkTrigram . match' -- attempt a single reduction
  where
    match' (OneOrMore r)  = match' r
    match' (Alt    r1 r2) = match' r1 `Or`  match' r2
    match' (Concat r1 r2) = match' r1 `And` match' r2
    match' _ = Any

