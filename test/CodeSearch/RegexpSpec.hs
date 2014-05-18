{-# LANGUAGE OverloadedStrings #-}

module CodeSearch.RegexpSpec (main, spec) where

import           CodeSearch.Regexp
import           CodeSearch.Types
import           Control.Monad     (forM_)
import           Data.Text         (pack)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let
    cases =
      [ ("a|b",    Single 'a' `Alt` Single 'b')
      , ("(ab)",   Single 'a' `Concat` Single 'b')
      , ("(ab)|c", (Single 'a' `Concat` Single 'b') `Alt` Single 'c')
      , ("[ab]",   Single 'a' `Alt` Single 'b')
      , ("a?",     ZeroOrOne (Single 'a'))
      , ("a*",     ZeroOrMore (Single 'a'))
      , ("a+",     OneOrMore (Single 'a'))
      , ("a?",     ZeroOrOne (Single 'a'))
      , ("ab",     Single 'a' `Concat` Single 'b')
      , ("abc|def",
          Alt (Single 'a' `Concat` Single 'b' `Concat` Single 'c')
              (Single 'd' `Concat` Single 'e' `Concat` Single 'f')
        )
      , ("Conduit",
          Single 'C' `Concat` Single 'o' `Concat` Single 'n' `Concat`
          Single 'd' `Concat` Single 'u' `Concat` Single 'i' `Concat`
          Single 't')
      ]

  describe "parseRegexp parses" $ do
    forM_ cases $ \(regexp, pattern) ->
      it regexp $ do
        parseRegexp (pack regexp) `shouldBe` Right pattern
