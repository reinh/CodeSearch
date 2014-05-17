{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module CodeSearch.Types.Expr
  ( Expr(..)
  , RegExpr
  ) where

import           Data.Data        (Data, Typeable)
import           Data.Foldable    (Foldable)
import           Data.Traversable (Traversable)

import Control.Lens

data Expr a
  = Empty
  | Single !a                  -- a
  | ZeroOrOne !(Expr a)        -- a?
  | ZeroOrMore !(Expr a)       -- a*
  | OneOrMore !(Expr a)        -- a+
  | Alt !(Expr a) !(Expr a)    -- a|b
  | Concat !(Expr a) !(Expr a) -- ab
  deriving (Eq,Ord,Show,Read,Data,Typeable,Functor,Foldable,Traversable)

type RegExpr = Expr Char

infixr 6 `Concat`
infixr 7 `Alt`
