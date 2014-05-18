module CodeSearch.RegexSearch (searchText) where

import           Data.Array                ((!))
import           Data.Text                 (Text)
import           Text.Regex.Base.RegexLike (MatchLength, MatchOffset,
                                            makeRegexM, matchAll)
import           Text.Regex.TDFA.Text      (Regex)

searchText :: Text -- ^ The regex
           -> Text -- ^ the source
           -> Either String [(MatchOffset, MatchLength)]
searchText rxpText txt = do
  rxp <- makeRegexM rxpText :: Either String Regex
  return $ fmap (! 0) (matchAll rxp txt)
