module CodeSearch.DocumentQuery
  ( query
  ) where

import           CodeSearch.Document      (Document(..))
import           CodeSearch.Index         (DocIndex, getA, queryIndex)
import           CodeSearch.Query         (Query (..))
import           CodeSearch.Query.Trigram (build)
import           CodeSearch.Regex.Expr    (RegExpr)
import           CodeSearch.Regex.Parser  (parseRegexp)

import           Control.Applicative
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as ISet
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text, pack)

query :: DocIndex -> Text -> Maybe (Set Document)
query idx rgx =
    fmap (ISet.foldl' update Set.empty)
         (parsedRegex >>= compile)
  where
    parsedRegex :: Maybe RegExpr
    parsedRegex = eitherToMaybe (parseRegexp rgx)
    update :: Set Document -> Int -> Set Document
    update a = Set.union a . maybe Set.empty Set.singleton . getA idx
    compile :: RegExpr -> Maybe IntSet
    compile = reduce . fmap (queryIndex idx . pack) . build

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- Reduce a Query over sets of documents to either a single set or Nothing,
-- which represents our inability to reduce the search space.
reduce :: Query IntSet -> Maybe IntSet
reduce q = case reduce' q of
             Any -> Nothing
             Val a -> Just a
             q' -> error ("Could not fully reduce " ++ show q')

reduce' :: Query IntSet -> Query IntSet
reduce' Any           = Any
reduce' (Val a)       = Val a
reduce' (Any `Or` _)  = Any
reduce' (_ `Or` Any)  = Any
reduce' (s `Or` s')   = ISet.union <$> reduce' s  <*> reduce' s'
reduce' (Any `And` s) = reduce' s
reduce' (s `And` Any) = reduce' s
reduce' (s `And` s')  = ISet.intersection <$> reduce' s <*> reduce' s'
