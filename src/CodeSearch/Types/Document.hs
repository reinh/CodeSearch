{-# LANGUAGE TemplateHaskell #-}

module CodeSearch.Types.Document
  ( Document(..)
  , path
  ) where

import           Control.Lens

data Document = Document
  { _path :: FilePath
  }
  deriving (Eq,Ord,Show,Read)

makeLenses ''Document
