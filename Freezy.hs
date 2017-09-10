{-| Freezy Interpreter Main -}
module Freezy where

import FreezyLexer

-- | The entry point to the interpreter
main :: IO ()
main = do
    print $ "Freezy v0.0.0"
    print $ runLexer initState $ lexIt "hallo fun plus(a,b){ a+b;}#aada"
