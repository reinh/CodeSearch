{-# LANGUAGE OverloadedStrings #-}

module CodeSearch.ResultSpec (main, spec) where

import           CodeSearch.Result

import           Data.Set          (Set,valid,fromList)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cartesian" $ do
    it "gives the cartesian product of two sets" $ do
      cartesian (fromList ["a", "b"]) (fromList ["c", "d"])
        `shouldBe` (fromList ["ac", "bc", "ad", "bd"] :: Set String)

      cartesian (fromList [""]) (fromList ["c", "d"])
        `shouldBe` (fromList ["c", "d"] :: Set String)
    it "always gives a valid set" $ do
      property $ \xs ys -> valid $ cartesian (fromList (xs :: [String]))
                                             (fromList (ys :: [String]))

  describe "parseRegexp" $ do
    it "handles empty" $ do
      parseRegexp "" `shouldBe` Empty
    it "handles single char" $ do
      parseRegexp "c" `shouldBe` Single 'c'
    it "handles concated chars" $ do
      parseRegexp "cc" `shouldBe` Single 'c' `Concat` Single 'c'
    it "handles zero or one char" $ do
      parseRegexp "c?" `shouldBe` ZeroOrOne (Single 'c')
    it "handles zero or more chars" $ do
      parseRegexp "c*" `shouldBe` ZeroOrMore (Single 'c')
    it "handles one or more chars" $ do
      parseRegexp "c+" `shouldBe` OneOrMore (Single 'c')
    it "handles alt chars" $ do
      parseRegexp "c|d" `shouldBe` Single 'c' `Alt` Single 'd'

  describe "trigrams'" $ do
    it "gives the trigrams for a string" $ do
      trigrams' ""      `shouldBe` Any
      trigrams' "a"     `shouldBe` Any
      trigrams' "ab"    `shouldBe` Any
      trigrams' "abc"   `shouldBe` Val "abc"
      trigrams' "abcd"  `shouldBe` Val "abc" `And` Val "bcd"
      trigrams' "abcde" `shouldBe` Val "abc" `And` Val "bcd" `And` Val "cde"
      trigrams' "abcdef" `shouldBe`
        Val "abc" `And` Val "bcd" `And` Val "cde" `And` Val "def"

  describe "trigrams" $ do
    it "folds the trigrams using `Or`" $ do
      trigrams (fromList []) `shouldBe` Any
      trigrams (fromList ["abc", "cde"]) `shouldBe`
        trigrams' "abc" `Or` trigrams' "cde"

  describe "reduceTrigrams" $ do
    it "reduces the trigram expression" $ do
      let a = Val ("abc" :: String)
          b = Val ("def" :: String)

      reduceTrigramExpr (Any `Or` Any)  `shouldBe` (Any :: TrigramExpr String)
      reduceTrigramExpr (Any `Or` a)   `shouldBe` Any
      reduceTrigramExpr (a   `Or` Any) `shouldBe` Any

      reduceTrigramExpr (Any `And` a)   `shouldBe` a
      reduceTrigramExpr (a   `And` Any) `shouldBe` a

      reduceTrigramExpr (a `Or` (a `And` b)) `shouldBe` a
      reduceTrigramExpr (a `Or` (b `And` a)) `shouldBe` a
      reduceTrigramExpr ((a `And` b) `Or` a) `shouldBe` a
      reduceTrigramExpr ((b `And` a) `Or` a) `shouldBe` a
