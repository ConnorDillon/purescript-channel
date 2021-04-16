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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
