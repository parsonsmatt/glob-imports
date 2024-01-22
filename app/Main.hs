module Main where

import GlobImports.Exe
import System.IO (readFile')
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        src : fileInput : dest : _ -> do
            contents <- readFile' fileInput
            spliceImports (Source src) (SourceContents contents) (Destination dest)
        _ ->
            fail . mconcat $
               [ "glob-imports: expected to be called with three arguments as an -F -pgmF preprocessor. got:"
               , show args
               ]


