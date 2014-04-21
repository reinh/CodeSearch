{-# LANGUAGE OverloadedStrings #-}

module CodeSearch.IndexSpec (main, spec) where

import           CodeSearch.Index

import           Data.ByteString  ()
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "buildIndex" $ do
    it "converts a bytestring into an index Map" $ do
      buildIndex "abcabc" `shouldBe` Map.fromList
        [("abc", Set.fromList [0,3])
        ,("bca", Set.fromList [1])
        ,("cab", Set.fromList [2])
        ]
