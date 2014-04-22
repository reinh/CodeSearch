{-# LANGUAGE DeriveDataTypeable #-}

module CodeSearch.Result
  ( Expr(..)
  , cartesian
  , emptyable
  , exact
  , prefix
  , suffix
  , match
  , parseRegexp
  , TrigramExpr(..)
  , trigrams
  , trigrams'
  , reduceTrigramExpr
  , reduceTrigram
  , transformWith
  , Transform(..)
  , inspect, debug
  , findFix
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Data hiding (Prefix)
import           Data.Foldable       (foldMap)
import           Data.Semigroup      hiding (Any)
import           Data.Set            (Set, singleton, union)
import qualified Data.Set            as Set

cartesian :: (Ord m, Monoid m) => Set m -> Set m -> Set m
cartesian xs ys = Set.fromList [x `mappend` y | x <- Set.toList xs, y <- Set.toList ys]

data Expr a
  = Empty
  | Single !a
  | ZeroOrOne !(Expr a)
  | ZeroOrMore !(Expr a)
  | OneOrMore !(Expr a)
  | Alt !(Expr a) !(Expr a)
  | Concat !(Expr a) !(Expr a)
  deriving (Eq,Ord,Read,Data,Typeable)

instance Show a => Show (Expr a) where
  show Empty = "Empty"
  show (Single a) = "Single " ++ show a
  show (ZeroOrOne e) = "ZeroOrOne " ++ show e
  show (ZeroOrMore e) = "ZeroOrMore " ++ show e
  show (OneOrMore e) = "OneOrMore " ++ show e
  show (s1 `Alt` s2) = "(" ++ show s1 ++ " `Alt` " ++ show s2 ++ ")"
  show (s1 `Concat` s2) = "(" ++ show s1 ++ " `Concat` " ++ show s2 ++ ")"

infixr `Alt`
infixr `Concat`

parseRegexp :: String -> Expr Char
parseRegexp = findFix reduceExpr . parseRegexp'
  where
    parseRegexp' (x:'?':xs) = ZeroOrOne  (Single x) `Concat` parseRegexp xs
    parseRegexp' (x:'*':xs) = ZeroOrMore (Single x) `Concat` parseRegexp xs
    parseRegexp' (x:'+':xs) = OneOrMore  (Single x) `Concat` parseRegexp xs
    parseRegexp' (x:'|':y:xs) = (Single x `Alt` Single y) `Concat` parseRegexp xs
    parseRegexp' (x:xs) = Single x `Concat` parseRegexp xs
    parseRegexp' [x] = Single x
    parseRegexp' [] = Empty

parse :: String -> TrigramExpr String
parse = transformWith Exact . parseRegexp

reduceExpr :: Expr a -> Expr a
reduceExpr (e `Concat` Empty) = e
reduceExpr (Empty `Concat` e) = e
reduceExpr e = e

emptyable :: Expr a -> Bool
emptyable Empty           = True
emptyable (Single     _)  = False
emptyable (ZeroOrOne  _)  = True
emptyable (ZeroOrMore _)  = True
emptyable (OneOrMore  e)  = emptyable e
emptyable (Alt    e1 e2)  = emptyable e1 || emptyable e2
emptyable (Concat e1 e2)  = emptyable e1 && emptyable e2

exact :: Expr Char -> Maybe (Set String)
exact Empty          = Just (singleton "")
exact (Single     c) = Just (singleton [c])
exact (ZeroOrOne  c) = union (singleton "") <$> exact c
exact (ZeroOrMore _) = Nothing
exact (OneOrMore  _) = Nothing
exact (Alt    e1 e2) = union <$> exact e1 <*> exact e2
exact (Concat e1 e2) = cartesian <$> exact e1 <*> exact e2

prefix :: Expr Char -> Set String
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

suffix :: Expr Char -> Set String
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

match :: Expr Char -> TrigramExpr String
match = reduceTrigram . match' -- attempt a single reduction
  where
    match' (OneOrMore r)  = match' r
    match' (Alt    r1 r2) = match' r1 `Or`  match' r2
    match' (Concat r1 r2) = match' r1 `And` match' r2
    match' _ = Any

data Transform = Prefix | Suffix | Exact

-- Information saving transformations
transformWith :: Transform -> Expr Char -> TrigramExpr String
transformWith trans = reduceTrigram . transformWith' trans
  where
    transformWith' Prefix = And <$> match <*> trigrams . prefix
    transformWith' Suffix = And <$> match <*> trigrams . suffix
    transformWith' Exact  = And <$> match <*> maybe Any trigrams . exact

data TrigramExpr a
  = Any
  | Val !a
  | Or  !(TrigramExpr a) !(TrigramExpr a)
  | And !(TrigramExpr a) !(TrigramExpr a)
  deriving (Eq,Ord,Show,Read,Data,Typeable)

infixr 6 `Or`
infixr 7 `And`

instance Plated (TrigramExpr a) where
  plate f (Or  a b) = Or <$> f a <*> f b
  plate f (And a b) = And <$> f a <*> f b
  plate _ a = pure a

trigrams' :: String -> TrigramExpr String
trigrams' []         = Any
trigrams' [_]        = Any
trigrams' [_,_]      = Any
trigrams' [a,b,c]    = Val [a,b,c]
trigrams' (a:b:c:xs) = Val [a,b,c] `And` trigrams' (b:c:xs)

instance Semigroup (TrigramExpr a) where
  (<>) = Or

trigrams :: Set String -> TrigramExpr String
trigrams = reduceTrigram . option Any id . foldMap (pure . trigrams') . Set.toList

reduceTrigram :: TrigramExpr String -> TrigramExpr String
reduceTrigram = findFix (transform reduceTrigramExpr)

reduceTrigramExpr :: TrigramExpr String -> TrigramExpr String

-- `Any` is an additive zero for `Or`
reduceTrigramExpr (Any `Or`  _  ) = Any
reduceTrigramExpr (_   `Or`  Any) = Any

-- `Any` is a multiplicative unit for `And`
reduceTrigramExpr (Any `And` x  ) = x
reduceTrigramExpr (x   `And` Any) = x

-- (("heL" & ("eLl" & "Llo")) | ("hel" & ("ell" & "llo")))
reduceTrigramExpr ((a `And` b) `Or` (c `And` d)) = (a `Or` c) `And` (b `Or` d)

-- -- A + AB = A = AB + A
reduceTrigramExpr (x `Or` (x' `And` _ )) | x == x' = x
reduceTrigramExpr (x `Or` (_  `And` x')) | x == x' = x
reduceTrigramExpr ((x `And` _)  `Or` x') | x == x' = x
reduceTrigramExpr ((_ `And` x') `Or` x ) | x == x' = x

-- -- AA = A, A+A = A
reduceTrigramExpr (a `And` a') | a == a' = a
reduceTrigramExpr (a `Or` a') | a == a' = a

-- -- AB + AC = A(B + C)
reduceTrigramExpr ((a `And` b) `Or` (a' `And` c)) | a == a' = a `And` (b `Or` c)

-- A = A
reduceTrigramExpr x = x

inspect :: TrigramExpr String -> String
inspect Any = "*"
inspect (Val s) = show s
inspect (Or s1 s2)  = "(" ++ inspect s1 ++ " | " ++ inspect s2 ++ ")"
inspect (And s1 s2) = "(" ++ inspect s1 ++ " & " ++ inspect s2 ++ ")"

debug = putStrLn . inspect

findFix :: Eq a => (a -> a) -> a -> a
findFix f x = let x' = f x in if x == x' then x else findFix f x'
