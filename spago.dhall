{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "now"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "routing"
  , "spec"
  , "spec-quickcheck"
  , "unordered-collections"
  , "purescript-html-parser-halogen"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
