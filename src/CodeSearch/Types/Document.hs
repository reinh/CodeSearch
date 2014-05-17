{-# LANGUAGE TemplateHaskell #-}

module CodeSearch.Types.Document
  ( Document(..)
  ) where

import           Control.Lens
import           Data.ByteString (ByteString)

newtype Document = Document { _path :: FilePath }
  deriving (Eq,Ord,Show,Read)

makeLenses ''Document

data DocumentResult = DocumentResult
  { _document :: Document
  , _locations :: [Int]
  }

makeLenses ''DocumentResult
