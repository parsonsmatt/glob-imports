-- | This preprocessor splices in imports to the file you define it in. Haskell
-- source files are discovered according to a glob relative to the file the code
-- is defined in. This utility is useful when metaprogramming with a group of
-- related files, where you want to use @TemplateHaskell@ or similar
--
-- By default, the glob is for all modules in the directory containing the
-- source file. Imports are qualified with the full module name to avoid
-- potential import conflicts. The pre-processor will splice in a top-level
-- value @_importedModules :: [String]@ which contains the fully qualified
-- names of the modules that were imported.
--
-- You may want to disable warnings for redundant imports, if you are only using
-- type class information. A future option to the library may only do empty
-- import lists, to only get access to type class instances.
--
-- As an example, consider the
-- <https://hackage.haskell.org/package/persistent-discover
-- @persistent-discover@> utility, which is inspired by @hspec-discover@. That
-- utility will perform the following transformation:
--
-- @
-- -- src/PersistentModels/All.hs
--
-- {-# OPTIONS_GHC -F -pgmF persistent-discover #-}
-- @
--
-- Then it will translate to:
--
-- @
-- -- src/PersistentModels/All.hs
--
-- module PersistentModels.All where
--
-- import PersistentModels.Foo ()
-- import PersistentModels.Bar ()
-- import PersistentModels.Baz ()
--
-- allEntityDefs :: [EntityDef]
-- allEntityDefs = $(discoverEntities)
-- @
--
-- With this package, we can generalize the overall pattern. The new source
-- module will look like this:
--
-- @
-- -- src/PersistentModels/All.hs
--
-- {-# OPTIONS_GHC -F -pgmF glob-imports #-}
--
-- module PersistentModels.All where
--
-- import Database.Persist.Sql
-- {- GLOB_IMPORTS_SPLICE -}
--
-- allEntityDefs :: [EntityDef]
-- allEntityDefs = $(discoverEntities)
-- @
--
-- This preprocessor will convert this into this form:
--
-- @
-- -- src/PersistentModels/All.hs
--
-- module PersistentModels.All where
--
-- import Database.Persist.Sql
-- import qualified PersistentModels.Foo
-- import qualified PersistentModels.Bar
-- import qualified PersistentModels.Baz
--
-- allEntityDefs :: [EntityDef]
-- allEntityDefs = $(discoverEntities)
-- @
--
-- Note how the only difference is that imports have been spliced in. This
-- allows you to more flexibly customize how the code works.
--
-- @since 0.1.0.0
module GlobImports.Exe where

import System.FilePath
import System.FilePath.Glob (Pattern, compile, match)
import Control.Monad (guard, filterM)
import Control.Monad.State
import Data.String
import Data.DList (DList(..))
import qualified Data.DList as DList
import Data.Foldable (for_)
import System.Directory
import Data.List
import Data.Char
import Control.Applicative
import Data.Maybe

-- | The source file location. This is the first argument passed to the
-- preprocessor.
newtype Source = Source { unSource :: FilePath }

-- | The source file contents. This is the 'String' contained in the file of the
-- second argument passed to the preprocessor.
newtype SourceContents = SourceContents { unSourceContents :: String }

-- | The destination file path to write the final source to. This is the third
-- argument passed to the preprocessor.
newtype Destination = Destination { unDestination :: FilePath }

data AllModelsFile = AllModelsFile
    { amfModuleBase :: Module
    , amfModuleImports :: [Module]
    }

data GlobOptions = GlobOptions { globOptionsSearchDir :: Maybe FilePath
                               , globOptionsPattern :: Maybe Pattern
                               }
                 deriving (Show)

instance Semigroup GlobOptions where
  (<>) l r = GlobOptions { globOptionsSearchDir = foldr (<|>) Nothing $ globOptionsSearchDir <$> [l, r]
                         , globOptionsPattern = foldr (<|>) Nothing $ globOptionsPattern <$> [l, r]
                         }

instance Monoid GlobOptions where
  mempty = GlobOptions { globOptionsSearchDir = Nothing
                       , globOptionsPattern = Nothing
                       }

parseOptions :: String -> GlobOptions
parseOptions s = GlobOptions { globOptionsSearchDir = searchDir s
                             , globOptionsPattern = globPattern s
                             }
  where
    getOption :: String -> String -> Maybe String
    getOption n s = fmap (drop (length n)) <$> mfirst $ filter (n `isPrefixOf`) (words s)

    searchDir :: String -> Maybe FilePath
    searchDir = getOption "search_dir="

    globPattern :: String -> Maybe Pattern
    globPattern s = compile <$> getOption "pattern=" s

    mfirst [] = Nothing
    mfirst (f : r) = Just f

extractOptions :: String -> GlobOptions
extractOptions str = mconcat $ parseOptions <$> optionLines
  where
    optionLines = filter ("GLOB_IMPORTS_OPTIONS" `isInfixOf`) (lines str)

-- |
--
-- @since 0.1.0.0
spliceImports
    :: Source
    -> SourceContents
    -> Destination
    -> IO ()
spliceImports (Source src) (SourceContents srcContents) (Destination dest) = do
    let
      opts = extractOptions srcContents
      (sourceDir, file) = splitFileName src
      searchDir = fromMaybe sourceDir $ globOptionsSearchDir opts
    files <- filter (/= file) <$> getFilesRecursive searchDir (globOptionsPattern opts)
    let
        input =
            AllModelsFile
                { amfModuleBase =
                    fromJust $ pathToModule src
                , amfModuleImports =
                    mapMaybe pathToModule (fmap (searchDir </>) files)
                }
        output =
            renderFile input srcContents

    writeFile dest output

-- | Returns a list of relative paths to all files in the given directory.
getFilesRecursive
    :: FilePath
    -- ^ The directory to search.
    -> Maybe Pattern
    -- ^ The glob pattern to filter with.
    -> IO [FilePath]
getFilesRecursive baseDir mpat = do
  files <- (sort <$> go [])
  pure $ case mpat of
           Just pat -> filter (match pat) files
           Nothing -> files
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)

renderFile
    :: AllModelsFile
    -> String
    -> String
renderFile amf originalContents =
    concatMap unlines
        [ modulePrior
        , newImportLines
        , newModuleRest
        ]
  where
    originalLines =
        lines originalContents

    (modulePrior, moduleRest) =
        case break ("GLOB_IMPORTS_SPLICE" `isInfixOf`) originalLines of
            (_, []) ->
                error $ unlines
                    [ "While processing the module, I was unable to find a comment with GLOB_IMPORTS_SPLICE."
                    , "I need this to know where to splice imports into the file. Please add a comment like "
                    , "this to the source file in the import section: "
                    , ""
                    , "-- GLOB_IMPORTS_SPLICE"
                    ]
            (prior, (globImportLine : rest)) ->
                (prior, rest)

    newModuleRest =
        let
            (remainingModule, lastImportLine) =
                break ("import" `isPrefixOf`) (reverse moduleRest)
            quoteModuleName mod' =
                "\"" <> moduleName mod' <> "\""
            mkFirstModuleLine mod' =
                "  [ " <> quoteModuleName mod'
            mkRestModuleLine mod' =
                "  , " <> quoteModuleName mod'
            newLines =
                reverse case amfModuleImports amf of
                    [] ->
                        []
                    (firstModule : restModules) ->
                        [ "_importedModules :: [String]"
                        , "_importedModules ="
                        , mkFirstModuleLine firstModule
                        ] <> map mkRestModuleLine restModules
                          <> [ "  ]"]
         in
            reverse (concat [remainingModule, newLines, lastImportLine])

    newImportLines =
        map (\mod' -> "import qualified " <> moduleName mod') (amfModuleImports amf)







--     render do
--     let
--         modName =
--             moduleName $ amfModuleBase amf
--     renderLine do
--         "{-# LINE 1 "
--         fromString $ show modName
--         " #-}"
--     "{-# LANGUAGE TemplateHaskell #-}"
--     ""
--     renderLine do
--         "module "
--         fromString $ modName
--         " where"
--     ""
--     for_ (amfModuleImports amf) \mod' ->
--         renderLine do
--             "import "
--             fromString $ moduleName mod'
--             " ()"
--     ""
--     "import Database.Persist.TH (discoverEntities)"
--     "import Database.Persist.Types (EntityDef)"
--     ""
--     "-- | All of the entity definitions, as discovered by the @glob-imports@ utility."
--     "allEntityDefs :: [EntityDef]"
--     "allEntityDefs = $(discoverEntities)"
--
-- -- -- | Derive module name from specified path.
-- -- pathToModule :: FilePath -> Module
-- -- pathToModule f =
-- --     Module
-- --         { moduleName =
-- --             intercalate "." $ mapMaybe go $ splitDirectories f
-- --         , modulePath =
-- --             f
-- --         }
-- --   where
-- --     go :: String -> Maybe String
-- --     go (c:cs) =
-- --         Just (toUpper c : cs)
-- --     fileName = last $ splitDirectories f
-- --     m:ms = takeWhile (/='.') fileName

-- |
data Module = Module
    { moduleName :: String
    , modulePath :: FilePath
    }
    deriving (Eq, Show)

mkModulePieces
    :: FilePath
    -> [String]
mkModulePieces fp = do
    let
        extension =
            takeExtension fp
    guard (extension == ".hs" || extension == ".lhs")
    reverse
        . takeWhile (not . isLowerFirst)
        . reverse
        . filter noDots
        . splitDirectories
        . dropExtension
        $ fp
  where
    noDots x =
        "." /= x && ".." /= x

isLowerFirst :: String -> Bool
isLowerFirst [] = True
isLowerFirst (c:_) = isLower c

pathToModule
    :: FilePath
    -> Maybe Module
pathToModule file = do
    case mkModulePieces file of
        [] ->
            empty
        x : xs ->  do
            guard $ all isValidModuleName (x : xs)
            pure Module
                { moduleName = intercalate "." (x:xs)
                , modulePath = file
                }

-- | Returns True if the given string is a valid task module name.
-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

-- | Returns True if the given Char is a valid taks module character.
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Convert a String in camel case to snake case.
casify :: String -> String
casify str = intercalate "_" $ groupBy (\a b -> isUpper a && isLower b) str

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix str =
    reverse <$> stripPrefix (reverse suffix) (reverse str)
