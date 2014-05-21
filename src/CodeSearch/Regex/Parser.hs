module CodeSearch.Regex.Parser (parseRegexp) where

import           CodeSearch.Regex.Expr

import Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text            (Text, unpack)

parseRegexp :: Text -> Either String RegExpr
parseRegexp = fmap shrinkExpr . parseOnly pRegex

alts, concats :: [Expr a] -> Expr a
alts = foldr1 Alt
concats = foldr1 Concat

pRegex :: Parser (Expr Char)
pRegex = alts <$> sepBy1 pBranch (char '|')

pBranch :: Parser RegExpr
pBranch = pAtom >>= pPostAtom

pAtom :: Parser (Expr Char)
pAtom = pGroup <|> pBracket <|> pChars

pPostAtom :: RegExpr -> Parser RegExpr
pPostAtom atom = (char '?' >> return (ZeroOrOne atom))
             <|> (char '*' >> return (ZeroOrMore atom))
             <|> (char '+' >> return (OneOrMore atom))
             <|> return atom

pGroup :: Parser (Expr Char)
pGroup = fmap concats $ char '(' *> manyTill pChar (char ')')

pBracket :: Parser (Expr Char)
pBracket = fmap alts $ char '[' *> manyTill pChar (char ']')

pChars :: Parser RegExpr
pChars = concats . fmap Single . unpack <$> takeTill special
  where special = inClass "?*+|"

pChar :: Parser (Expr Char)
pChar = Single <$> anyChar
