{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "node-fs-aff"
  , "optparse"
  , "psci-support"
  , "string-parsers"
  , "stringutils"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
