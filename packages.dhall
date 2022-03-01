let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220228/packages.dhall
        sha256:585403682c9378a55da644fb2edbc74d2592d18283bc9fa3457ad79398bb55bb

in  upstream
  with node-fs.repo = "https://github.com/JordanMartinez/purescript-node-fs"
  with node-fs.version = "addCopyFile"
  with node-fs-aff.repo
       = "https://github.com/JordanMartinez/purescript-node-fs-aff"
  with node-fs-aff.version = "addCopyFile"
  with argparse-basic =
    { dependencies =
      [ "arrays"
      , "console"
      , "debug"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "free"
      , "lists"
      , "maybe"
      , "node-process"
      , "record"
      , "strings"
      , "transformers"
      ]
    , repo = "https://github.com/natefaubion/purescript-argparse-basic.git"
    , version = "v1.0.0"
    }
