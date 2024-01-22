# glob-imports

![build status](https://github.com/parsonsmatt/glob-imports/actions/workflows/haskell.yml/badge.svg)

A Haskell source  preprocessor which generalizes `persistent-discover` and
`hspec-discover`, allowing you to metaprogram with source files.

A big limitation of `TemplateHaskell` is that it cannot modify the import list
of a file. The only way to metaprogram with the imports is to use a source
preprocessor like this.


## As an executable

Let's say you've got a ton of `persistent` database models, all defined in a
module hierarchy like this:

```
src/
  Models/
    Foo.hs
    Bar.hs
    Baz.hs
    Blargh.hs
    What.hs
    OhNo.hs
```

If you're using `persistent` to automatically generate migrations, you'll want
to have all the `[EntityDef]` in scope from each module. You can do that by
importing each module that defines models and calling `$(discoverEntities)`,
introduced in `persistent-2.13`.

But you may forget to import a module.

This utility splices in a qualified import statement for *all* the modules defined in the
current directory and sub-directories, along with a list of imported modules. From this, you can use
[`discover-instances`](https://hackage.haskell.org/package/discover-instances)
or other `TemplateHaskell` code to work with these.

To use it, place the following command in a file, located in the same directory
that your Haskell modules are in:

```haskell
-- src/Models/All.hs

{-# OPTIONS_GHC -F -pgmF glob-imports #-}

module Models.All where

import Database.Persist.Sql

allEntities :: [EntityDef]
allEntities = $(discoverEntities)
```

The preprocessor will glob files in the current directory, and import all of
them qualified. The result would look like this:

```haskell
-- src/Models/All.hs

-- post-processed

module Models.All where

import qualified Models.Foo
import qualified Models.Bar
import qualified Models.Baz
import qualified Models.Blargh
import qualified Models.What
import qualified Models.OhNo
import Database.Persist.Sql

_importedModules :: [String]
_importedModules =
    [ "Models.Foo"
    , "Models.Bar"
    , "Models.Baz"
    , "Models.Blargh"
    , "Models.What"
    , "Models.OhNo"
    ]

allEntities :: [EntityDef]
allEntities = $(discoverEntities)
```

You'll need to add `glob-imports` to the `build-tool-depends` on your cabal file
for this to work.
