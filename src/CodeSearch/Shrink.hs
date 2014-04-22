module CodeSearch.Shrink
  ( shrinkTrigramExpr
  , shrinkTrigram
  ) where

import           CodeSearch.Types
import           CodeSearch.Util

import Control.Lens

shrinkTrigram :: TrigramExpr String -> TrigramExpr String
shrinkTrigram = findFix (transform shrinkTrigramExpr)

shrinkTrigramExpr :: TrigramExpr String -> TrigramExpr String

-- `Any` is an additive zero for `Or`
shrinkTrigramExpr (Any `Or`  _  ) = Any
shrinkTrigramExpr (_   `Or`  Any) = Any

-- `Any` is a multiplicative unit for `And`
shrinkTrigramExpr (Any `And` x  ) = x
shrinkTrigramExpr (x   `And` Any) = x

-- (("heL" & ("eLl" & "Llo")) | ("hel" & ("ell" & "llo")))
shrinkTrigramExpr ((a `And` b) `Or` (c `And` d)) = (a `Or` c) `And` (b `Or` d)

-- -- A + AB = A = AB + A
shrinkTrigramExpr (x `Or` (x' `And` _ )) | x == x' = x
shrinkTrigramExpr (x `Or` (_  `And` x')) | x == x' = x
shrinkTrigramExpr ((x `And` _)  `Or` x') | x == x' = x
shrinkTrigramExpr ((_ `And` x') `Or` x ) | x == x' = x

-- -- AA = A, A+A = A
shrinkTrigramExpr (a `And` a') | a == a' = a
shrinkTrigramExpr (a `Or` a') | a == a' = a

-- -- AB + AC = A(B + C)
shrinkTrigramExpr ((a `And` b) `Or` (a' `And` c)) | a == a' = a `And` (b `Or` c)

-- A = A
shrinkTrigramExpr x = x
