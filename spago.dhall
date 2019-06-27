{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "pureswift"
, dependencies =
    [ "console"
    , "corefn"
    , "debug"
    , "lists"
    , "node-fs-aff"
    , "ordered-collections"
    , "pathy"
    , "prelude"
    , "prettier-printer"
    , "psci-support"
    , "spec"
    , "spec-discovery"
    , "trie"
    , "unicode"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
