{-# LANGUAGE DeriveDataTypeable #-}

module CodeSearch.Result
  ( foldTrigrams
  , reduceTrigrams
  , trigrams
  , TrigramExpr(..)
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Data
import           Data.Foldable       (foldMap)
import           Data.Semigroup      hiding (Any)

-- data Rule
--   = Empty
--   | Single Char
--   | ZeroOrOne Char
--   | ZeroOrMore Char
--   | OneOrMore Char
--   | Alt Char Char
--   | Concat Char Char

data TrigramExpr
  = Any
  | Val !String
  | Or  !TrigramExpr !TrigramExpr
  | And !TrigramExpr !TrigramExpr
  deriving (Eq,Ord,Show,Read,Data,Typeable)

infixr `Or`
infixr `And`

instance Plated TrigramExpr where
  plate f (Or  a b) = Or <$> f a <*> f b
  plate f (And a b) = And <$> f a <*> f b
  plate _ a = pure a

trigrams :: String -> TrigramExpr
trigrams []         = Any
trigrams [_]        = Any
trigrams [_,_]      = Any
trigrams [a,b,c]    = Val [a,b,c]
trigrams (a:b:c:xs) = Val [a,b,c] `And` trigrams (b:c:xs)

instance Semigroup TrigramExpr where
  (<>) = Or

foldTrigrams :: [String] -> TrigramExpr
foldTrigrams = reduceTrigrams . option Any id . foldMap (pure . trigrams)

reduceTrigrams :: TrigramExpr -> TrigramExpr
reduceTrigrams = transform reducer
  where
    -- `Any` is a zero
    reducer (Any `Or`  _  ) = Any
    reducer (_   `Or`  Any) = Any
    reducer (Any `And` _  ) = Any
    reducer (_   `And` Any) = Any

    -- A + AB = A
    reducer (x `Or` (x' `And` _ )) | x == x' = x
    reducer (x `Or` (_  `And` x')) | x == x' = x
    reducer ((x `And` _)  `Or` x') | x == x' = x
    reducer ((_ `And` x') `Or` x ) | x == x' = x

    -- (A + B)(A + C) = A + BC
    reducer ((x `Or` y) `And` (x' `Or` z )) | x == x' = x `Or` (y `And` z)
    reducer ((y `Or` x) `And` (z  `Or` x')) | x == x' = x `Or` (y `And` z)

    -- A = A
    reducer x             = x
