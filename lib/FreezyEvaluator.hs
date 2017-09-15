{-| FreezyEvaluator - The Evaluator for Freezy Expressions -}
module FreezyEvaluator where

import FreezyLang
import FreezyOperators

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M
import Prelude hiding (lookup)

{- * Evaluator Types -}

{- ** The Environment -}

-- | Constructor for the global environment
globalEnv :: Env
globalEnv = Env Nothing M.empty

-- | Opens a new environment inside of env
openScope :: Env -> Env
openScope env = Env (Just env) M.empty


-- | Assignment. This doesn't allow mutation.
assign :: String -> FreezyValue -> Evaluator FreezyValue
assign name value = do
    env <- get
    case M.lookup name (scope env) of
        Just _ -> throwError $ EvaluatorError ("already assigned" ++ name) 0
        Nothing -> do
            put $ env { scope = M.insert name value (scope env)}
            return value

lookup :: String -> Evaluator FreezyValue
lookup name = do
    env <- get
    lookup' name env
  where
    lookup' :: String -> Env -> Evaluator FreezyValue
    lookup' n e =
        case M.lookup n (scope e) of
            Just x -> return x
            Nothing ->
                case enclosing e of
                   Just enc -> lookup' n enc
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
evaluate (Fn args body) = do
    env <- get
    return $ Lambda env args body
evaluate (Fun name args body) = do
    env <- get
    let fn = Function name env args body
    assign (t_lexeme name) fn
evaluate (Call callee args) = do
    calleeRes <- evaluate callee
    argsRes <- mapM evaluate args
    case calleeRes of
        fn@(Lambda closure argList body) -> do
            env <- get
            put (openScope closure)
            insertArgs argList argsRes
            funRes <- evaluateBlock Nothing body
            put env -- reset the env
            return funRes
        fn@(Function name closure argList body) -> do
            env <- get
            put (openScope closure)
            assign (t_lexeme name) fn -- allow recursion
            insertArgs argList argsRes
            funRes <- evaluateBlock Nothing body
            put env -- reset the env
            return funRes
        _ -> throwError $ EvaluatorError "can only call functions" 0
evaluate (Let token expr) = evaluate expr >>= (assign (t_lexeme token))
evaluate (Const token) = lookup (t_lexeme token)
evaluate (Grouping expr) = evaluate expr
evaluate (Block exprs) = evaluateBlock Nothing exprs
evaluate (Print expr) = do
    res <- evaluate expr
    let stringified = stringify res
    liftIO $ putStrLn (stringified)
    return $ String stringified
evaluate expr = throwError $ EvaluatorError ("cannot evaluate" ++ show expr) 0

-- | Helper function to evaluate a function body/block expr
evaluateBlock :: Maybe FreezyValue -> [Expr] -> Evaluator FreezyValue
evaluateBlock retVal [] = do
    case retVal of
        Nothing -> throwError $ EvaluatorError ("Empty blocks are not allowed") 0
        Just val -> return val
evaluateBlock retVal (x:xs) = do
  retVal' <- evaluate x
  evaluateBlock (Just retVal') xs

-- | Helper function to put the arguments in the environment
insertArgs :: [Token] -> [FreezyValue] -> Evaluator ()
insertArgs argList args = do
    let zipped = zip (map t_lexeme argList) args
    mapM_ (uncurry assign) zipped

{- * Operators -}

performUnaryOp :: Token-> FreezyValue -> Evaluator FreezyValue
performUnaryOp (Token BANG _ _) operand = return $ unaryNot operand
performUnaryOp (Token MINUS _ _) (Number n)= return $ unaryMinus (Number n)
performUnaryOp _ _ = throwError $ EvaluatorError "Strange Unary Operation" 0

performOp op left right
     | t_type op `elem` [PLUS, MINUS, STAR, DASH, GR_EQ, LE_EQ, GR, LE] = numericOp (t_type op) left right
     | otherwise = polyOp (t_type op) left right

numericOp :: TokenType -> FreezyValue -> FreezyValue -> Evaluator FreezyValue
numericOp op a@(Number _) b@(Number _)
  | op == PLUS = return $ plus a b
  | op == MINUS = return $ minus a b
  | op == STAR = return $ times a b
  | op == DASH = return $ division a b
  | op == GR = return $ greater a b
  | op == GR_EQ = return $ greaterEqual a b
  | op == LE = return $ lesser a b
  | op == LE_EQ = return $ lesserEqual a b
numericOp _ _ _ = throwError $ EvaluatorError "Expect Numbers" 0

polyOp :: TokenType -> FreezyValue -> FreezyValue -> Evaluator FreezyValue
polyOp op a b
   | op == TILDE = return $ concatenate a b
   | op == EQ_EQ = return $ equal a b
   | op == BANG_EQ = return $ notEqual a b
   | otherwise = throwError $ EvaluatorError "How did we get here?" 0


{- * Helper Functions -}

-- | Converts a literal token to its runtime representation
litToVal :: Token -> FreezyValue
litToVal (Token NUMBER lexeme _) = Number (read lexeme)
litToVal (Token STRING lexeme _) = String lexeme
litToVal (Token TRUE _ _)        = Boolean True
litToVal (Token FALSE _ _)       = Boolean False
