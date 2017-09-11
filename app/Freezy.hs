{-| Freezy Interpreter Main -}
module Freezy where

import FreezyLexer

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
            print $ runLexer initState $ lexIt input' -- print / eval
            repl -- loop
  where prompt = "freezy─➤➤➤"
