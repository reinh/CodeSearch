module CodeSearch.Document
  ( Version
  , Document(..)
  ) where

import           Control.Applicative
import           Data.Bytes.Serial
import           Data.Text           (Text)

type Version = Text

data Document =
    Document { _pName :: Text, _pVersion :: Version, _path :: FilePath }
  deriving (Eq,Ord,Show,Read)

instance Serial Document where
  serialize (Document pn v p) = do
    serialize pn
    serialize v
    serialize p
  deserialize = Document <$> deserialize <*> deserialize <*> deserialize
