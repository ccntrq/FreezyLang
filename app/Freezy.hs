{-| Freezy Interpreter Main -}
module Freezy where

import FreezyLexer
import FreezyParser
import FreezyEvaluator

import System.Console.Readline
import System.Exit

-- | The entry point to the interpreter
main :: IO ()
main = do
    print $ "Freezy v0.0.0"
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

-- | interpret a given source string
runSource :: String -> IO ()
runSource source = do
    let lexed = runLexer initState $ lexIt source
    case lexed of
        (Right tokens, st)-> do
            let parsed = runParser (initParserState tokens) expression
            case parsed of
                (Right expr, st) -> do
                    eval <- runEvaluator globalEnv $ evaluate expr
                    case eval of
                      Right res -> print res
                      Left err -> print err
                (Left err, st) -> print err
        (Left err, st)-> print err
