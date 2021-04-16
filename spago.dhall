{ name = "record-extra-srghma"
, dependencies =
    [ "record"
    , "typelevel-prelude"
    , "unfoldable"
    , "control"
    , "assert"
    , "lists"
    , "parallel"
    , "js-timers"
    , "arrays"
    , "console"
    , "effect"
    , "functions"
    , "maybe"
    , "prelude"
    , "transformers"
    , "tuples"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
