{-| Freezy Main -}
module Main where

import FreezyMain

-- | Just a stupid wrapper to work around a bug with 'cabal haddock --executables'
main :: IO()
main = entry
