module Types where

import Data.Set (Set)

type PackageMeta =
  { version :: String
  , repo :: String
  , dependencies :: Array String
  }

type PackageInfo =
  { version :: String
  , repo :: String
  , dependencies :: Set String
  }
