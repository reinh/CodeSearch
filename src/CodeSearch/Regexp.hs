module CodeSearch.Regexp (parseRegexp) where

import           CodeSearch.Expr
import           CodeSearch.Types

import Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text            (Text)

parseRegexp :: Text -> Either String RegExpr
parseRegexp = fmap shrinkExpr . parseOnly pRegex

alts = foldr1 Alt
concats = foldr1 Concat

pRegex = alts <$> sepBy1 pBranch (char '|')

pBranch = pAtom >>= pPostAtom

pAtom = pGroup <|> pBracket <|> pChar

pPostAtom :: RegExpr -> Parser RegExpr
pPostAtom atom = (char '?' >> return (ZeroOrOne atom))
             <|> (char '*' >> return (ZeroOrMore atom))
             <|> (char '+' >> return (OneOrMore atom))
             <|> return atom

pGroup = fmap concats $ char '(' *> manyTill pChar (char ')')

pBracket = fmap alts $ char '[' *> manyTill pChar (char ']')

pChar = Single <$> anyChar
