{-# LANGUAGE OverloadedStrings #-}

module CodeSearch.IndexSpec (main, spec) where

import Test.Hspec
import CodeSearch.Index
import Data.ByteString ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "buildNgrams" $ do
    it "converts a bytestring into ngrams" $ do
      buildNgrams "abcd" `shouldBe` [("abc", 0), ("bcd", 1)]
