module CodeSearch.DocumentQuery
  ( query
  ) where

import           CodeSearch.Index    (DocIndex, queryIndex)
import           CodeSearch.Regexp   (parseRegexp)
import           CodeSearch.Trigrams (build)
import           CodeSearch.Types    (Document, Query (..), RegExpr)

import           Control.Applicative
import           Data.Set            (Set, intersection, union)
import           Data.Text           (Text, pack)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

query :: DocIndex -> Text -> Maybe (Set Document)
query idx txt = eitherToMaybe (parseRegexp txt) >>= compile
  where
    compile :: RegExpr -> Maybe (Set Document)
    compile = reduce . fmap (queryIndex idx . pack) . build

-- Reduce a Query over sets of documents to either a single set or Nothing,
-- which represents our inability to reduce the search space.
reduce :: Query (Set Document) -> Maybe (Set Document)
reduce q = case reduce' q of
             Any -> Nothing
             Val a -> Just a
             q' -> error ("Could not fully reduce " ++ show q')

reduce' :: Query (Set Document) -> Query (Set Document)
reduce' Any           = Any
reduce' (Val a)       = Val a
reduce' (Any `Or` _)  = Any
reduce' (_ `Or` Any)  = Any
reduce' (s `Or` s')   = union <$> reduce' s  <*> reduce' s'
reduce' (Any `And` s) = reduce' s
reduce' (s `And` Any) = reduce' s
reduce' (s `And` s')  = intersection <$> reduce' s <*> reduce' s'
