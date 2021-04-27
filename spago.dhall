{ name = "channel"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "contravariant"
  , "aff"
  , "avar"
  , "newtype"
  , "control"
  , "exceptions"
  , "assert"
  , "either"
  , "foldable-traversable"
  , "lazy"
  , "maybe"
  , "prelude"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
