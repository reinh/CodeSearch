{-# LANGUAGE TemplateHaskell #-}

module CodeSearch.Types.Document
  ( Document(..)
  ) where

import Distribution.Package
import Distribution.Version
import           Control.Lens
--import           Data.ByteString (ByteString)

type Repo = String

data Document = Document { _repo :: Repo, _pName :: PackageName, _pVersion :: Version, _path :: FilePath }
  deriving (Eq,Ord,Show,Read)
