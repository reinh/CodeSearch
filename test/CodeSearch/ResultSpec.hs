{-# LANGUAGE OverloadedStrings #-}

module CodeSearch.ResultSpec (main, spec) where

import           CodeSearch.Result

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "trigrams" $ do
    it "gives the trigrams for a string" $ do
      trigrams ""      `shouldBe` Any
      trigrams "a"     `shouldBe` Any
      trigrams "ab"    `shouldBe` Any
      trigrams "abc"   `shouldBe` Val "abc"
      trigrams "abcd"  `shouldBe` Val "abc" `And` Val "bcd"
      trigrams "abcde" `shouldBe` Val "abc" `And` Val "bcd" `And` Val "cde"
      trigrams "abcdef" `shouldBe`
        Val "abc" `And` Val "bcd" `And` Val "cde" `And` Val "def"

  describe "foldTrigrams" $ do
    it "folds the trigrams using `Or`" $ do
      foldTrigrams [] `shouldBe` Any
      foldTrigrams ["abc", "cde"] `shouldBe`
        trigrams "abc" `Or` trigrams "cde"
      foldTrigrams ["abcd", "cdef", "defg"] `shouldBe`
        trigrams "abcd" `Or` trigrams "cdef" `Or` trigrams "defg"
      foldTrigrams ["ab", "cdef", "defg"] `shouldBe` Any

  describe "reduceTrigrams" $ do
    it "reduces the trigram expression" $ do
      let a = Val "abc"
          b = Val "def"
          c = Val "ghi"
      reduceTrigrams (Any `And` a) `shouldBe` Any
      reduceTrigrams (a `And` Any) `shouldBe` Any
      reduceTrigrams (a `Or` (a `And` b)) `shouldBe` a
      reduceTrigrams (a `Or` (b `And` a)) `shouldBe` a
      reduceTrigrams ((a `And` b) `Or` a) `shouldBe` a
      reduceTrigrams ((b `And` a) `Or` a) `shouldBe` a
      reduceTrigrams ((a `Or` b) `And` (a `Or` c)) `shouldBe`
        a `Or` (b `And` c)
      reduceTrigrams ((b `Or` a) `And` (c `Or` a)) `shouldBe`
        a `Or` (b `And` c)
