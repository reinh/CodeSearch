module CodeSearch.Index
  ( Index(..)
  , DocIndex
  , getA
--  , mapIndex
  , queryIndex
  , singleton
  ) where

import           CodeSearch.Types

import           Control.Applicative
import           Data.Map.Strict  (Map, union)
import qualified Data.Map.Strict  as Map
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Maybe       (fromMaybe, fromJust)
import           Data.Monoid
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as ISet
import           Data.Text        (Text, drop, length, take)
import           Prelude          hiding (drop, length, take)
import           Data.Bytes.Serial

type DocIndex = Index Document

type Trigram = Text

data Index a =
    Index (Bimap a Int) (Map Trigram IntSet)
  deriving (Eq, Show)

instance (Ord a, Serial a) => Serial (Index a) where
  serialize (Index s i) = serialize (Bimap.toList s) >> serialize i
  deserialize = Index <$> (Bimap.fromList <$> deserialize) <*> deserialize

getA :: Ord a => Index a -> Int -> Maybe a
getA (Index s _) i = Bimap.lookupR i s

repInt :: Map Int Int -> Map Trigram IntSet -> Map Trigram IntSet
repInt l = Map.map (ISet.map (\v -> fromJust $ Map.lookup v l))

{-mapIndex :: (Ord a, Ord b) => (a -> b) -> Index a -> Index b
mapIndex f (Index s idx) =
    let (t, ns) = Map.foldlWithKey (\(t, i) d o ->
                                        let n = f d
                                            ci = Map.insertWith 
                                        in (Map.insert )) s
    in Index ns (repInt t idx)
-}
instance Ord a => Monoid (Index a) where
  mempty = Index Bimap.empty mempty
  mappend (Index s idx) (Index s' idx') =
    let newS = Bimap.fold (\a b c -> Bimap.tryInsert a (Bimap.size c) c) s s'
        newIdx = (Map.unionWith ISet.union idx (Map.map (ISet.map (\i -> fromJust (Bimap.lookupR i s' >>= (flip Bimap.lookup $ newS)))) idx'))
    in newS `seq` newIdx `seq` Index newS newIdx

singleton :: Document -> Text -> DocIndex
singleton = addToIndex mempty

queryIndex :: DocIndex -> Trigram -> IntSet
queryIndex (Index _ idx) tri = fromMaybe ISet.empty (Map.lookup tri idx)

addToIndex :: DocIndex -> Document -> Text -> DocIndex
addToIndex idx doc txt | length trigram < 3 = idx
                       | otherwise = addToIndex (idx <> single) doc nexts
  where
    trigram = take 3 txt
    nexts   = drop 1 txt
    single  = Index (Bimap.singleton doc 0) (Map.singleton trigram (ISet.singleton 0))
