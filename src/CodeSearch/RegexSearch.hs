module CodeSearch.RegexSearch (searchText) where

import           Data.Array                ((!))
import           Data.Text                 (Text)
import           Text.Regex.Base.RegexLike (MatchLength, MatchOffset,
                                            makeRegexM, matchOnce)
import           Text.Regex.TDFA.Text      (Regex)

searchText :: Text -> Text -> Either String (MatchOffset, MatchLength)
searchText rxpText txt = do
  rxp <- makeRegexM rxpText :: Either String Regex
  match <- matchOnce rxp txt `note` "No match"
  return (match ! 0)

note :: Maybe a -> String -> Either String a
note Nothing str = Left str
note (Just x) _ = Right x
