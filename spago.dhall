{ name = "purescript-rrb-list"
, dependencies =
  [ "arrays"
  , "control"
  , "foldable-traversable"
  , "functions"
  , "maybe"
  , "nonempty"
  , "partial"
  , "prelude"
  , "tailrec"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
