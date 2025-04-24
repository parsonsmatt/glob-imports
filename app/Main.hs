module Main where

import GlobImports.Exe
import Control.Applicative (optional)
import Options.Applicative
import System.IO (readFile')

data Args = Args
  { argsOriginalFile :: FilePath
  , argsSourceFile :: FilePath
  , argsDest :: FilePath
  , argsSearchDir :: Maybe FilePath
  , argsPattern :: String
  } deriving (Show)

parseArgs =
  execParser $
  info (Args
        <$> originalFileParser
        <*> sourceFileParser
        <*> destFileParser
        <*> searchDirParser
        <*> patternParser) mempty
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
          <> metavar "PATTERN"
          <> help "Pattern on which to match file paths"
          <> value "**/*.hs"

main :: IO ()
main = do
    args <- parseArgs
    contents <- readFile' $ argsSourceFile args
    spliceImports
        (Source $ argsSourceFile args)
        (SourceContents $ contents)
        (Destination $ argsDest args)
        (argsSearchDir args)
        (argsPattern args)
