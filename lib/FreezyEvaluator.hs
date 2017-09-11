{-| FreezyEvaluator - The Evaluator for Freezy Expressions -}
module FreezyEvaluator where

import FreezyLang

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map as M

{-| * Evaluator Types -}

{-| ** The Environment -}

data Env = Env
    { env :: M.Map String FreezyValue
    , enclosing :: Maybe Env
    } deriving (Show)

-- | Constructor for the global environment
globalEnv :: Env
globalEnv = Env M.empty Nothing

{-| ** Evaluator Stack -}

-- | Evaluator Monad
type Evaluator a = ReaderT Env (ExceptT EvaluatorError IO) a

-- | Unwrapping of the monad stack
runEvaluator :: Env -> Evaluator a -> IO (Either EvaluatorError a)
runEvaluator env ev = runExceptT $ runReaderT ev env

-- | Type for Evaluator errors
data EvaluatorError = EvaluatorError
    { reason :: String
    , e_line :: Int
    } deriving (Show)

{- * Evalution Logic -}

evaluate :: Expr -> Evaluator FreezyValue
evaluate (Literal token) = return $ litToVal token
evaluate (Binary left op right) = do
    leftRes <- evaluate left
    rightRes <- evaluate right
    performOp op leftRes rightRes

{- * Operators -}

performOp op left right
     | t_type op `elem` [PLUS, MINUS, STAR, DASH] = numericOp (t_type op) left right

numericOp :: TokenType -> FreezyValue -> FreezyValue -> Evaluator FreezyValue
numericOp op (Number a) (Number b)
  | op == PLUS = return $ Number (a + b)
  | op == MINUS = return $ Number (a - b)
  | op == STAR = return $ Number (a * b)
  | op == DASH = return $ Number (a `div` b)
numericOp _ _ _ = throwError $ EvaluatorError "Expect Numbers" 0



{- * Helper Functions -}

-- | Converts a literal token to its runtime representation
litToVal :: Token -> FreezyValue
litToVal (Token NUMBER lexeme _) = Number (read lexeme)
litToVal (Token STRING lexeme _) = String lexeme
litToVal (Token TRUE _ _)        = Boolean True
litToVal (Token FALSE _ _)       = Boolean False
