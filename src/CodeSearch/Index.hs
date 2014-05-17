module CodeSearch.Index
  ( Index
  , DocIndex
  , singleton
  , mapIndex
  ) where

import           CodeSearch.Types

import           Control.Lens    hiding (Index)
import           Data.Map.Strict (Map, union)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Monoid
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text, drop, length, take)
import           Prelude         hiding (drop, length, take)

type DocIndex = Index Document

type Trigram = Text

newtype Index a = Index (Map Trigram (Set a))
  deriving (Eq, Show, Read)

mapIndex :: Ord b => (a -> b) -> Index a -> Index b
mapIndex f (Index idx) = Index $ (Map.map . Set.map) f idx

instance Monoid (Index a) where
  mempty = Index Map.empty
  mappend (Index idx) (Index idx') = Index (idx `union` idx')

singleton :: Document -> Text -> DocIndex
singleton = addToIndex mempty

queryIndex :: DocIndex -> Trigram -> Set Document
queryIndex (Index idx) tri = fromMaybe Set.empty (Map.lookup tri idx)

addToIndex :: DocIndex -> Document -> Text -> DocIndex
addToIndex (Index idx) doc txt = add
  where
    trigram = take 3 txt
    add | length trigram < 3 = Index idx
        | otherwise = addToIndex (Index $ union idx (Map.singleton trigram (Set.singleton doc)))
                                 doc
                                 (drop 1 txt)
