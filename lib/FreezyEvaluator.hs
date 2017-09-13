{-| FreezyEvaluator - The Evaluator for Freezy Expressions -}
module FreezyEvaluator where

import FreezyLang

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M
import Prelude hiding (lookup)

{- * Evaluator Types -}

{- ** The Environment -}

-- | Constructor for the global environment
globalEnv :: Env
globalEnv = M.empty

-- | Assignment. This doesn't allow mutation.
assign :: String -> FreezyValue -> Evaluator FreezyValue
assign name value = do
    env <- get
    case M.lookup name env of
          Just _ -> throwError $ EvaluatorError ("already assigned" ++ name) 0
          Nothing -> do
              put $ M.insert name value env
              return value

lookup :: String -> Evaluator FreezyValue
lookup name = do
    env <- get
    case M.lookup name env of
          Just x -> return x
          Nothing -> throwError $ EvaluatorError ("Cannot find in env: " ++ name) 0

{- ** Evaluator Stack -}

-- | Evaluator Monad
type Evaluator a = StateT Env (ExceptT EvaluatorError IO) a

-- | Unwrapping of the monad stack
runEvaluator :: Env -> Evaluator a -> IO (Either EvaluatorError (a, Env))
runEvaluator env ev = runExceptT $ runStateT ev env

-- | Type for Evaluator errors
data EvaluatorError = EvaluatorError
    { reason :: String
    , e_line :: Int
    } deriving (Show)

{- * Evalution Logic -}

evaluateIt :: Program -> Evaluator ()
evaluateIt = mapM_ evaluate

evaluate :: Expr -> Evaluator FreezyValue
evaluate (Unary op operand) = do
    operandRes <- evaluate operand
    performUnaryOp op operandRes
evaluate (Binary left op right) = do
    leftRes <- evaluate left
    rightRes <- evaluate right
    performOp op leftRes rightRes
evaluate (Literal token) = return $ litToVal token
evaluate (IfExpr condE thenE elseE) = do
    condRes <- evaluate condE
    if isTruthy condRes
        then evaluate thenE
        else evaluate elseE
evaluate (Fun args body) = do
    env <- get  -- closure?
    return $ Function env args body
evaluate (Call callee args) = do
    calleeRes <- evaluate callee
    argsRes <- mapM evaluate args
    case calleeRes of
        (Function closure argList body) -> do
            env <- get
            put (insertArgs argList argsRes closure)
            funRes <- evaluateBody calleeRes body -- urgh... we shouldn't pass the calleeRes here
            put env -- reset the env
            return funRes
        _ -> throwError $ EvaluatorError "can only call functions" 0
evaluate (Let token expr) = evaluate expr >>= (assign (t_lexeme token))
evaluate (Const token) = lookup (t_lexeme token)
evaluate (Grouping expr) = evaluate expr
evaluate (Print expr) = do
    res <- evaluate expr
    let stringified = stringify res
    liftIO $ print (stringified)
    return $ String stringified
evaluate expr = throwError $ EvaluatorError ("cannot evaluate" ++ show expr) 0

-- | Helper function to evaluate a function body
evaluateBody :: FreezyValue -> [Expr] -> Evaluator FreezyValue
evaluateBody retVal [] = return retVal
evaluateBody retVal (x:xs) = do
  retVal' <- evaluate x
  evaluateBody retVal' xs

-- | Helper function to put the arguments in the environment
insertArgs :: [Token] -> [FreezyValue] -> Env -> Env
insertArgs argList args env =
    let zipped = zip argList args
    in foldl insertFn env zipped
  -- don't use assign here. it will break the whaky shadowing
  where insertFn acc (k, v) = M.insert (t_lexeme k) v acc

-- | Stringifications for FreezyValues. Currently this just wraps 'show'
stringify :: FreezyValue -> String
stringify = show

-- | Checks wether a given FreezyValue is truthy. In Freezy everything besides
--   '0' and 'false' is considered to be truthy
isTruthy :: FreezyValue -> Bool
isTruthy (Number 0)      = False
isTruthy (Boolean False) = False
isTruthy _               = True

{- * Operators -}

performUnaryOp :: Token-> FreezyValue -> Evaluator FreezyValue
performUnaryOp (Token BANG _ _) operand = return $ Boolean (isTruthy operand)
performUnaryOp (Token MINUS _ _) (Number n)= return $ Number (-n)
performUnaryOp _ _ = throwError $ EvaluatorError "Strange Unary Operation" 0


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
