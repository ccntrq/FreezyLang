{-| FreezyParser - A Parser for FreezyLang -}
module FreezyParser where

import FreezyLang

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

import Control.Monad.Loops (anyM)

{- * Parser Types -}

-- | Parser Monad
type Parser a = ExceptT ParserError (StateT ParserState Identity) a

-- | Unwrapping of the monad stack
runParser :: ParserState -> Parser a -> (Either ParserError a, ParserState)
runParser st l = runIdentity $ runStateT (runExceptT l) st

-- | State of the parser
data ParserState = ParserState
    { tokens :: [Token]
    , previousTokens :: [Token]
    } deriving Show

-- | Initiate the parse state with a list of tokens
initParserState tokens = ParserState tokens []

-- | Type for parser errors
data ParserError = ParserError
    { reason :: String
    , e_line :: Int
    } deriving (Show)

{- * Main Parser Logic -}

-- | Entry point to the recursive descent expression parser
expression :: Parser Expr
expression = concatenation

concatenation :: Parser Expr
concatenation = binaryParser equality [TILDE]

equality :: Parser Expr
equality = binaryParser comparison [BANG_EQ, EQ_EQ]

comparison :: Parser Expr
comparison = binaryParser term [GR_EQ, LE_EQ, GR, LE]

term :: Parser Expr
term = binaryParser factor [PLUS, MINUS]

factor :: Parser Expr
factor = binaryParser unary [STAR, DASH]

unary :: Parser Expr
unary = do
    matches <- match [MINUS, BANG]
    if matches
        then do
            op <- previous
            operand <- unary
            return $ Unary op operand
        else
            ifExpr

ifExpr :: Parser Expr
ifExpr = do
    matches <- match [IF]
    if matches
        then do
            cond <- expression
            consume THEN "then expected"
            thenExpr <- expression
            consume ELSE "else expected"
            elseExpr <- expression
            return $ IfExpr cond thenExpr elseExpr
        else assignment

assignment :: Parser Expr
assignment = do
    matches <- match [LET]
    if matches
        then do
            name <- consume IDENTIFIER "Expect variable name"
            consume EQUAL "Expect equal"
            val <- expression
            return $ Let name val
        else printExpr

printExpr :: Parser Expr
printExpr = do
    matches <- match [PRINT]
    if matches
        then do
            expr <- expression
            return $ Print expr
        else call

call :: Parser Expr
call = do
    expr <- fun
    finishCallLoop expr
  where
    finishCallLoop :: Expr -> Parser Expr
    finishCallLoop expr = do
        matches <- match [LPAR]
        if matches
            then do
                call <- finishCall [] expr
                finishCallLoop call
            else return expr
    finishCall :: [Expr] -> Expr -> Parser Expr
    finishCall acc callee = do
       matches <- match [RPAR]
       if matches
           then return $ Call callee acc
           else do
             arg <- expression
             matches' <- match [COMMA]
             if matches'
                 then finishCall (acc ++ [arg]) callee
                 else do
                     consume RPAR "expect closing Parentheses"
                     return $ Call callee (acc ++ [arg])

fun :: Parser Expr
fun = do
    matches <- match [FUN]
    if matches
        then do
            _ <- consume LPAR "Expect opening parentheses"
            args <- argParser []
            _ <- consume LBRACE "Expect opening brace"
            body <- bodyParser []
            return $ Fun args body
        else primary
  where
    argParser :: [Token] -> Parser [Token]
    argParser acc = do
        matches <- match [IDENTIFIER]
        if matches
            then do
                arg <- previous
                _ <- match [COMMA] -- optional commas?
                argParser (acc ++ [arg])
            else do
                consume RPAR "Excpect closing parens"
                return acc
    bodyParser :: [Expr] -> Parser [Expr]
    bodyParser acc = do
        expr <- expression
        matches <- match [RBRACE]
        if matches
            then return (acc ++ [expr])
            else bodyParser (acc ++ [expr])


primary :: Parser Expr
primary = do
   cur <- peek
   case t_type cur of
       IDENTIFIER -> do
           advance
           return $ Const cur
       LPAR -> do
           expr <- expression
           consume RPAR "Expect closing paren"
           return $ Grouping expr
       _ ->
           if isLitToken cur
              then do
                  advance
                  return $ Literal cur
              else throwError $ ParserError ("Cannot parse " ++ show cur) (t_line cur)
  where
    isLitToken cur = t_type cur `elem` [TRUE, FALSE, STRING, NUMBER]

-- | generate a Binary parser for the given operators
binaryParser :: Parser Expr -> [TokenType] -> Parser Expr
binaryParser next ops = do
    expr <- next
    loop expr
  where
    loop left = do
        matches <- match ops
        if matches
            then do
                op <- previous
                right <- expression
                loop $ Binary left op right
            else return left


{- * Parsing Primitives -}

-- | check if we are at the end
isAtEnd :: Parser Bool
isAtEnd = do
    st <- get
    case tokens st of
        ((Token EOF _ _)):xs -> return True
        _ -> return False

-- | advance the state by one token
advance :: Parser Token
advance = do
    atEnd <- isAtEnd
    if atEnd
        then throwError $ ParserError "Unexpected End of Input"  0 -- EOF line XXX
        else do
            st <- get
            put st { tokens = tail $ tokens st
                   , previousTokens = (head $ tokens st):(previousTokens st)
                   }
            previous


-- | advance if the current token matches any of the given TokenTypes
match :: [TokenType] -> Parser Bool
match ttypes = do
    matches <- anyM check ttypes
    if matches
        then do
            advance
            return True
        else return False

-- | check if the current token matches the given TokenType
check :: TokenType -> Parser Bool
check ttype = do
    atEnd <- isAtEnd
    if atEnd
        then return False
        else do
            cur <- peek
            return $ t_type cur == ttype

-- | consume the current token if it matches the given TokenType or throw an error
consume :: TokenType -> String -> Parser Token
consume ttype errMsg = do
    matches <- check ttype
    if matches
        then advance
        else throwError $ ParserError errMsg 0 -- XXX

-- | returns the current token
peek :: Parser Token
peek = do
    st <- get
    case tokens st of
        (x:xs) -> return x
        _ -> throwError $ ParserError "Out of tokens" 0 -- XXX

-- | returns the previous token
previous :: Parser Token
previous = do
    st <- get
    case previousTokens st of
        (x:xs) -> return x
        _ -> throwError $ ParserError "No previous token" 0 -- XXX
