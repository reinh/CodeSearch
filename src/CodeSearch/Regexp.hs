module CodeSearch.Regexp
  (
  ) where

import CodeSearch.Expr
import CodeSearch.Types
import CodeSearch.Util

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
