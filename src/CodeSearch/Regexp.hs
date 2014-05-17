module CodeSearch.Regexp
  ( parseRegexp
  ) where

import           CodeSearch.Expr
import           CodeSearch.Types

parseRegexp :: String -> RegExpr
parseRegexp = shrinkExpr . parseRegexp'
  where
    parseRegexp' (x:'?':xs) = ZeroOrOne  (Single x) `Concat` parseRegexp xs
    parseRegexp' (x:'*':xs) = ZeroOrMore (Single x) `Concat` parseRegexp xs
    parseRegexp' (x:'+':xs) = OneOrMore  (Single x) `Concat` parseRegexp xs
    parseRegexp' (x:'|':y:xs) = (Single x `Alt` Single y) `Concat` parseRegexp xs
    parseRegexp' (x:xs) = Single x `Concat` parseRegexp xs
    parseRegexp' [] = Empty
