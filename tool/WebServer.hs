module Main where

import Data.Conduit (($=), ($$))
import qualified Data.Conduit.List as CL
import qualified Data.Bytes.Serial as B
import qualified Data.Bytes.Put as B
import qualified Data.ByteString.Lazy as BSL
import CodeSearch.LoadHackage

main :: IO ()
main = do
  ps <- loadAvailablePackages
  idx <- enumPackages ourPackages $= CL.iterM print $= processPackages $$ buildIndex
  
  return ()
