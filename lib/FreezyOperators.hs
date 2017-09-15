{- | Implementation of the operators for Freezy -}
module FreezyOperators where

import FreezyLang

unaryMinus :: FreezyValue -> FreezyValue
unaryMinus (Number n) = Number (-n)
unaryMinus _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

unaryNot :: FreezyValue -> FreezyValue
unaryNot = Boolean . not . isTruthy

plus :: FreezyValue -> FreezyValue -> FreezyValue
plus (Number a) (Number b) = Number (a + b)
plus _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

minus :: FreezyValue -> FreezyValue -> FreezyValue
minus (Number a) (Number b) = Number (a - b)
minus _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

times :: FreezyValue -> FreezyValue -> FreezyValue
times (Number a) (Number b) = Number (a * b)
times _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

division :: FreezyValue -> FreezyValue -> FreezyValue
division (Number a) (Number b) = Number (a `div` b)
division _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

greater :: FreezyValue -> FreezyValue -> FreezyValue
greater (Number a) (Number b) = Boolean (a > b)
greater _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

greaterEqual :: FreezyValue -> FreezyValue -> FreezyValue
greaterEqual (Number a) (Number b) = Boolean (a >= b)
greaterEqual _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

lesser :: FreezyValue -> FreezyValue -> FreezyValue
lesser (Number a) (Number b) = Boolean (a < b)
lesser _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

lesserEqual :: FreezyValue -> FreezyValue -> FreezyValue
lesserEqual (Number a) (Number b) = Boolean (a <= b)
lesserEqual _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

equal :: FreezyValue -> FreezyValue -> FreezyValue
equal a b = Boolean (a == b)

notEqual :: FreezyValue -> FreezyValue -> FreezyValue
notEqual a b = Boolean (a /= b)

concatenate :: FreezyValue -> FreezyValue -> FreezyValue
concatenate a b = String ((stringify a) ++ (stringify b))

stringify :: FreezyValue -> String
stringify (String s) = s
stringify (Number n) = show n
stringify (Boolean n) = show n
stringify (Function _ _ _) = "__^__" -- meh

-- | Checks wether a given FreezyValue is truthy. In Freezy everything besides
--   '0' and 'false' is considered to be truthy
isTruthy :: FreezyValue -> Bool
isTruthy (Number 0)      = False
isTruthy (Boolean False) = False
isTruthy _               = True

