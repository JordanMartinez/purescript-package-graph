module Types where

type PackageMeta =
  { version :: String
  , repo :: String
  , dependencies :: Array String
  }
