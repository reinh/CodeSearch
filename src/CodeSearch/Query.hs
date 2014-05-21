{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module CodeSearch.Query
  ( Query(..)
  , TrigramQuery
  ) where

import           Control.Applicative
import           Control.Lens        (Plated, Traversable, plate)
import           Control.Monad
import           Data.Data           (Data, Typeable)
import           Data.Foldable       (Foldable)
import           Data.Semigroup      hiding (Any)
import           Data.Traversable    ()

data Query a
  = Any
  | Val !a
  | Or  !(Query a) !(Query a)
  | And !(Query a) !(Query a)
  deriving (Eq,Ord,Show,Read,Data,Typeable,Functor,Foldable,Traversable)

type TrigramQuery = Query String

instance Monad Query where
  return = Val
  Val a   >>= f = f a
  Any     >>= _ = Any
  Or a b  >>= f = Or  (a >>= f) (b >>= f)
  And a b >>= f = And (a >>= f) (b >>= f)

instance Applicative Query where
  pure = return
  (<*>) = ap

infixr 6 `Or`
infixr 7 `And`

instance Plated (Query a) where
  plate f (Or  a b) = Or <$> f a <*> f b
  plate f (And a b) = And <$> f a <*> f b
  plate _ a = pure a

instance Semigroup (Query a) where
  (<>) = Or
