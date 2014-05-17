module CodeSearch.Query
  ( buildQuery
  ) where

import CodeSearch.Types
import CodeSearch.Trigrams

buildQuery :: RegExpr -> TrigramQuery
buildQuery = transformWith Exact
