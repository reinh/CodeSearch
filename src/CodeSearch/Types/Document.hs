{-# LANGUAGE TemplateHaskell #-}

module CodeSearch.Types.Document
  ( Version, Document(..)
  ) where

import Data.Text (Text)
import Distribution.Package
import           Control.Lens

type Version = Text

data Document =
    Document { _pName :: PackageName, _pVersion :: Version, _path :: FilePath }
  deriving (Eq,Ord,Show,Read)
