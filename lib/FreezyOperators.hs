{- | Implementation of the operators for Freezy -}
module FreezyOperators where

import FreezyLang
{- * Numeric Operators. The Typechecking is done in the Evaluator -}

{- ** Unary -}

-- | unary Minus is the only unary numeric op in Freezy
unaryMinus :: FreezyValue -> FreezyValue
unaryMinus (Number n) = Number (-n)
unaryMinus _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

{- ** Binary Math -}

-- | adds to numbers
plus :: FreezyValue -> FreezyValue -> FreezyValue
plus (Number a) (Number b) = Number (a + b)
plus _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

-- | subtract one number from another
minus :: FreezyValue -> FreezyValue -> FreezyValue
minus (Number a) (Number b) = Number (a - b)
minus _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

-- | multiply two numbers
times :: FreezyValue -> FreezyValue -> FreezyValue
times (Number a) (Number b) = Number (a * b)
times _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

-- | perform integer division
division :: FreezyValue -> FreezyValue -> FreezyValue
division (Number a) (Number b) = Number (a `div` b)
division _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

{- ** Binary Comparison -}

-- | Check wether a number is greater than another
--
--   * only numbers can be compared in Freezy
greater :: FreezyValue -> FreezyValue -> FreezyValue
greater (Number a) (Number b) = Boolean (a > b)
greater _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

-- | Check wether a number is greater or equal than another
--
--   * only numbers can be compared in Freezy
greaterEqual :: FreezyValue -> FreezyValue -> FreezyValue
greaterEqual (Number a) (Number b) = Boolean (a >= b)
greaterEqual _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

-- | Check wether a number is lesser than another
--
--   * only numbers can be compared in Freezy
lesser :: FreezyValue -> FreezyValue -> FreezyValue
lesser (Number a) (Number b) = Boolean (a < b)
lesser _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

-- | Check wether a number is lesser or equal than another
--
--   * only numbers can be compared in Freezy
lesserEqual :: FreezyValue -> FreezyValue -> FreezyValue
lesserEqual (Number a) (Number b) = Boolean (a <= b)
lesserEqual _ _ = error "THIS SHOULD NOT HAPPEN AT ALL!"

{- * Polymorphic Operators -}

-- | Falsify a value. See isTruthy for a definition of what is considered to be true
unaryNot :: FreezyValue -> FreezyValue
unaryNot = Boolean . not . isTruthy

-- | Equality Checks Functions and Lambdas are not comparable everything else
--   is compared by value
equal :: FreezyValue -> FreezyValue -> FreezyValue
equal a b = Boolean (a == b)

-- | Check wether two values are unequal
notEqual :: FreezyValue -> FreezyValue -> FreezyValue
notEqual a b = Boolean (a /= b)

-- | (Stringify) and concatenate two values
concatenate :: FreezyValue -> FreezyValue -> FreezyValue
concatenate a b = String ((stringify a) ++ (stringify b))

{- * Helper -}

-- | Turn a value into a string. Functions and Lambdas are not really stringifiable
stringify :: FreezyValue -> String
stringify (String s) = s
stringify (Number n) = show n
stringify (Boolean n) = show n
stringify (Function name _ _ _) = "__fun " ++ (t_lexeme name) ++ "__" -- meh
stringify (Lambda _ _ _) = "__anon__" -- meh

-- | Checks wether a given FreezyValue is truthy. In Freezy everything besides
--   '0' and 'false' is considered to be truthy
isTruthy :: FreezyValue -> Bool
isTruthy (Number 0)      = False
isTruthy (Boolean False) = False
isTruthy _               = True

