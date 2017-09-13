{-| Freezy Interpreter Main -}
module Freezy where

import FreezyLexer
import FreezyParser
import FreezyEvaluator

import System.Console.Readline
import System.Environment
import System.Exit

{-* MAIN -}

-- | The main
main :: IO ()
main = do
    args <- getArgs
    dispatch args

-- | the arg dispatcher
dispatch :: [String] -> IO ()
dispatch ["-v"] = version
dispatch ["-h"] = usage
dispatch []     = replEntry
dispatch files =
   if length files == 1
       then execFile $ head files
       else usage

-- | print the version
version :: IO ()
version = putStrLn "Freezy v0.0.3"

-- | print version and usage
usage :: IO ()
usage = do
    version
    putStrLn "Usage: freezy [filename] [-v] [-h]\n"

{- * REPL -}

-- | prints a welcome message and enters the repl
replEntry :: IO ()
replEntry = do
    version
    putStrLn "Welcome to the Freezy repl."
    putStrLn "Enter 'quit' or hit Ctrl+D to leave"
    repl -- enter the repl

-- | a repl for Freezy using readline
repl :: IO ()
repl = do
    input <- readline prompt -- read
    case input of
        Nothing     -> exitSuccess
        Just "quit" -> exitSuccess
        Just input' -> do
            addHistory input'
            runSource input'
            repl
  where prompt = "freezy─➤➤➤"

{- * execFile -}

execFile :: String -> IO ()
execFile name = do
    source <- readFile name
    runSource source
    exitSuccess

{- * Backend -}

-- | interpret a given source string. This is the backend for both the repl
--   and for execFile
runSource :: String -> IO ()
runSource source = do
    let lexed = runLexer initState $ lexIt source
    case lexed of
        (Right tokens, st)-> do
            let parsed = runParser (initParserState tokens) parseIt
            case parsed of
                (Right prog, st) -> do
                    eval <- runEvaluator globalEnv $ evaluateIt prog
                    case eval of
                      Right res -> print res
                      Left err -> print err
                (Left err, st) -> print err
        (Left err, st)-> print err
