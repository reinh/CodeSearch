module CodeSearch.Index
  ( buildNgrams
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Map        (Map)
import qualified Data.Map        as Map

asdkljha asdfkljh

buildNgrams :: ByteString -> [(ByteString, Int)]
buildNgrams = go 0
  where
    go c xs | BS.length xs < 3 = []
            | otherwise        = (BS.take 3 xs, c) : go (c + 1) (BS.drop 1 xs)
