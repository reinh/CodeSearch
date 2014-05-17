module CodeSearch.Index
  ( buildIndex
  , queryIndex
  ) where

import           CodeSearch.Types

import           Control.Applicative ((<$>), (<*>))
import           Data.ByteString     (ByteString, drop, length, take)
import           Data.Map.Strict     (Map, alter, empty, unionWith, union)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set, insert, singleton)
import qualified Data.Set            as Set
import           Prelude             hiding (drop, length, take)
import Data.List (foldl')

type Index = Map Trigram DocIndex

type DocIndex = Map Document TrigramIndex

type Trigram = ByteString
type TrigramIndex = Set Int

newtype QueryResult a = QueryResult (Maybe a)
  deriving (Eq,Ord,Show,Read)

queryIndex :: Index -> Trigram -> Maybe DocIndex
queryIndex = flip Map.lookup

buildIndex :: [(Document,ByteString)] -> Index
buildIndex = foldl' go empty
  where
    go m (d,bs) = unionWith union m (buildPartialIndex d bs)

buildPartialIndex :: Document -> ByteString -> Index
buildPartialIndex doc bs = invert doc (buildTrigramIndex bs)

invert :: Ord b => a -> Map b c -> Map b (Map a c)
invert = flip Map.foldlWithKey empty . go
  where
    go :: Ord b => a -> Map b (Map a c) -> b -> c -> Map b (Map a c)
    go a m b c = Map.insert b (Map.singleton a c) m

buildTrigramIndex :: ByteString -> Map Trigram TrigramIndex
buildTrigramIndex = buildTrigramIndex' empty 0

buildTrigramIndex' :: Map Trigram TrigramIndex -- The accumulated posting list
                   -> Int                      -- The current column
                   -> ByteString               -- The remaining bytestring
                   -> Map Trigram TrigramIndex -- The final posting list
buildTrigramIndex' m c xs
  | length xs  < 3 = m
  | otherwise     = buildTrigramIndex' (update m) (c + 1) (drop 1 xs)
    where
      update = alter (Just . add c) (take 3 xs)
      add = maybe <$> singleton <*> insert


