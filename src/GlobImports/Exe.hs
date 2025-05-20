{-# LANGUAGE OverloadedStrings #-}

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
-- {\-# OPTIONS_GHC -F -pgmF persistent-discover #-\}
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
-- {\-# OPTIONS_GHC -F -pgmF glob-imports #-\}
--
-- module PersistentModels.All where
--
-- import Database.Persist.Sql
-- {\- GLOB_IMPORTS_SPLICE -\}
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

import Control.Applicative
import Control.Monad (guard, when)
import Control.Monad.State
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Foldable (for_)
import Data.List
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.Process.Typed

-- | The source file location. This is the first argument passed to the
-- preprocessor.
newtype Source = Source {unSource :: FilePath}

-- | The source file contents. This is the 'String' contained in the file of the
-- second argument passed to the preprocessor.
newtype SourceContents = SourceContents {unSourceContents :: String}

-- | The destination file path to write the final source to. This is the third
-- argument passed to the preprocessor.
newtype Destination = Destination {unDestination :: FilePath}

data AllModelsFile = AllModelsFile
    { amfModuleBase :: Module
    , amfModuleImports :: [Module]
    }

printDebug :: Bool -> String -> IO ()
printDebug enabled str = when enabled $ putStrLn ("[DEBUG] " ++ str)

-- |
--
-- @since 0.1.0.0
spliceImports
    :: Source
    -> SourceContents
    -> Destination
    -> Maybe FilePath
    -> String
    -> [String]
    -> Bool
    -> IO ()
spliceImports (Source src) (SourceContents srcContents) (Destination dest) msearchDir pat prefixes debug = do
    let
        (sourceDir, _file) = splitFileName src
        searchDir = fromMaybe sourceDir msearchDir
        excludePrefixFilter target = not $ any (`isPrefixOf` target) prefixes

    printDebug debug $ "searching directory: " ++ searchDir
    printDebug debug $ "searching with pattern: " ++ pat
    printDebug debug $ "excluding file name: " ++ src
    eitherFiles <- fmap (filter (essentiallyDistinct src)) <$> getFiles searchDir pat
    files <- case eitherFiles of
        Left e -> error e
        Right f -> pure f
    let
        filteredFiles = filter excludePrefixFilter files
    printDebug debug $ "including files:\n" ++ intercalate "\n|  " filteredFiles
    let
        input =
            AllModelsFile
                { amfModuleBase =
                    fromJust $ pathToModule src
                , amfModuleImports =
                    mapMaybe pathToModule (fmap (searchDir </>) filteredFiles)
                }
        output =
            renderFile input srcContents

    writeFile dest output
    where
      essentiallyDistinct :: FilePath -> FilePath -> Bool
      essentiallyDistinct l r = simplifyPath (Text.pack l) /= simplifyPath (Text.pack r)

      simplifyPath :: Text -> Text
      simplifyPath str =
        let
          intermediate = Text.replace "//" "/" str
        in
          if intermediate == str
          then intermediate
          else simplifyPath intermediate

-- | Returns a list of relative paths to all files in the given directory.
getFiles
    :: FilePath
    -- ^ The glob pattern to filter with.
    -> String
    -- ^ The directory to search.
    -> IO (Either String [FilePath])
getFiles baseDir pat = do
    (exitCode, out, err) <- readProcess $ proc "find" [baseDir, "-wholename", pat]
    pure $ case exitCode of
        ExitSuccess -> Right . lines . Text.unpack . decodeUtf8 . LBS.toStrict $ out
        ExitFailure _ -> Left . Text.unpack . decodeUtf8 . LBS.toStrict $ err

renderFile
    :: AllModelsFile
    -> String
    -> String
renderFile amf originalContents =
    concatMap
        unlines
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
                error $
                    unlines
                        [ "While processing the module, I was unable to find a comment with GLOB_IMPORTS_SPLICE."
                        , "I need this to know where to splice imports into the file. Please add a comment like "
                        , "this to the source file in the import section: "
                        , ""
                        , "-- GLOB_IMPORTS_SPLICE"
                        ]
            (prior, (_globImportLine : rest)) ->
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
                        ]
                            <> map mkRestModuleLine restModules
                            <> ["  ]"]
         in
            reverse (concat [remainingModule, newLines, lastImportLine])

    newImportLines =
        map (\mod' -> "import qualified " <> moduleName mod') (amfModuleImports amf)

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
isLowerFirst (c : _) = isLower c

pathToModule
    :: FilePath
    -> Maybe Module
pathToModule file = do
    case mkModulePieces file of
        [] ->
            empty
        x : xs -> do
            guard $ all isValidModuleName (x : xs)
            pure
                Module
                    { moduleName = intercalate "." (x : xs)
                    , modulePath = file
                    }

-- | Returns True if the given string is a valid task module name.
-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c : cs) = isUpper c && all isValidModuleChar cs

-- | Returns True if the given Char is a valid taks module character.
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Convert a String in camel case to snake case.
casify :: String -> String
casify str = intercalate "_" $ groupBy (\a b -> isUpper a && isLower b) str

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix str =
    reverse <$> stripPrefix (reverse suffix) (reverse str)
