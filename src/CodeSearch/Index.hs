module CodeSearch.Index
  ( buildIndex
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.ByteString     (ByteString, drop, length, take)
import           Data.Map.Strict     (Map, alter, empty)
import           Data.Set            (Set, insert, singleton)
import           Prelude             hiding (drop, length, take)

buildIndex :: ByteString -> Map ByteString (Set Int)
buildIndex = buildIndex' empty 0

buildIndex' :: Map ByteString (Set Int) -- The accumulated posting list
            -> Int                      -- The current column
            -> ByteString               -- The remaining bytestring
            -> Map ByteString (Set Int) -- The final posting list
buildIndex' m c xs
  | length xs < 3 = m
  | otherwise     = buildIndex' (update m) (c + 1) (drop 1 xs)
    where
      update = alter (Just . add c) (take 3 xs)
      add = maybe <$> singleton <*> insert
