{ name = "purescript-rrb-list"
, dependencies =
  [ "control"
  , "foldable-traversable"
  , "functions"
  , "maybe"
  , "nonempty"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "tailrec"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
