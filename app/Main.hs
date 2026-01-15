module Main where

import GlobImports.Exe
import Options.Applicative
import System.IO (readFile')

data Args = Args
    { argsOriginalFile :: FilePath
    , argsSourceFile :: FilePath
    , argsDest :: FilePath
    , argsSearchDir :: Maybe FilePath
    , argsPattern :: String
    , argsExcludedPrefixes :: String
    , argsDebug :: Bool
    , argsImportQualified :: Affix
    }
    deriving (Show)

parseArgs :: IO Args
parseArgs =
    execParser $
        info
            ( Args
                <$> originalFileParser
                <*> sourceFileParser
                <*> destFileParser
                <*> searchDirParser
                <*> patternParser
                <*> excludedPrefixesParser
                <*> debugParser
                <*> importQualifiedParser
            )
            mempty
  where
    originalFileParser = argument str (metavar "ORIGINAL_FILE")
    sourceFileParser = argument str (metavar "SOURCE_FILE")
    destFileParser = argument str (metavar "DEST_FILE")
    searchDirParser =
        optional . (option str) $
            long "search-dir"
                <> metavar "SEARCH_DIR"
                <> help "Directory to search for files in"
    patternParser =
        option str $
            long "pattern"
                <> metavar "PATTERN[,PATTERN...]"
                <> help "Patterns on which to match file paths (comma separated)"
                <> value "**/*.hs"
    excludedPrefixesParser =
        option str $
            long "exclude-prefixes"
                <> metavar "PREFIX[,PREFIX...]"
                <> help "File path prefixes to exclude (comma separated)"
                <> value ""
    debugParser =
        switch $
            long "debug"
                <> help "Whether to print debug output"
    stringToAffix x = case x of
        "pre" -> Just Prefix
        "post" -> Just Suffix
        _ -> Nothing
    importQualifiedParser =
        option (maybeReader stringToAffix) $
            long "import-qualified"
                <> metavar "AFFIX"
                <> help "pre: import qualified M, post: import M qualified"
                <> value Prefix

main :: IO ()
main = do
    args <- parseArgs
    contents <- readFile' $ argsSourceFile args
    spliceImports
        (Source $ argsSourceFile args)
        (SourceContents $ contents)
        (Destination $ argsDest args)
        (argsSearchDir args)
        (splitOn ',' (argsPattern args))
        (splitOn ',' (argsExcludedPrefixes args))
        (argsDebug args)
        (argsImportQualified args)
  where
    splitOn _ [] = []
    splitOn sep xs =
        takeWhile (/= sep) xs : splitOn sep (drop 1 $ dropWhile (/= sep) xs)
