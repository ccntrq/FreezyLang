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
initParserState :: [Token] -> ParserState
initParserState toks = ParserState toks []

-- | Type for parser errors
data ParserError = ParserError
    { reason :: String
    , e_line :: Int
    } deriving (Show)

{- * Main Parser Logic -}

-- | Parser expression until it errors out or reaches EOF
parseIt :: Parser Program
parseIt = parseIt' []
  where
    parseIt' acc = do
        eof <- isAtEnd
        if eof
            then return acc
            else do
                expr <- expression
                _ <- match [SEMICOLON] -- optional semicolon
                parseIt' $ acc ++ [expr]

-- | Entry point to the recursive descent expression parser
--   We go from lowest precedence up to the highest
expression :: Parser Expr
expression = concatenation

-- | Parse a concatenation operation
--   This is the lowest precedence operation in Freezy
concatenation :: Parser Expr
concatenation = binaryParser equality [TILDE]

-- | Equality Check Parser
equality :: Parser Expr
equality = binaryParser comparison [BANG_EQ, EQ_EQ]

-- | Comparison Parser
comparison :: Parser Expr
comparison = binaryParser term [GR_EQ, LE_EQ, GR, LE]

-- | Term Parser
term :: Parser Expr
term = binaryParser factor [PLUS, MINUS]

-- | Factor Parser
factor :: Parser Expr
factor = binaryParser unary [STAR, DASH]

-- | Unary Operation Parser
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

-- | Parses an if Expr
--   If is an Expr in Freezy meaning that both a then expr and a else expr is
--   mandatory
ifExpr :: Parser Expr
ifExpr = do
    matches <- match [IF]
    if matches
        then do
            cond <- expression
            _ <- consume THEN "then expected"
            thenExpr <- expression
            _ <- consume ELSE "else expected"
            elseExpr <- expression
            return $ IfExpr cond thenExpr elseExpr
        else assignment

-- | Parses a 'let' assignment
assignment :: Parser Expr
assignment = do
    matches <- match [LET]
    if matches
        then do
            name <- consume IDENTIFIER "Expect variable name"
            _ <- consume EQUAL "Expect equal"
            val <- expression
            return $ Let name val
        else printExpr

-- | Parses a print expr. print returns the stringified value
printExpr :: Parser Expr
printExpr = do
    matches <- match [PRINT]
    if matches
        then do
            expr <- expression
            return $ Print expr
        else call

-- | Parses a call expr. In Freezy you can call either:
--
--   * a Lambda
--   * or a named Function
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
                callRes <- finishCall [] expr
                finishCallLoop callRes
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
                     _ <- consume RPAR "expect closing Parentheses"
                     return $ Call callee (acc ++ [arg])

-- | Parse a named function declaration
fun :: Parser Expr
fun = do
    matches <- match [FUN]
    if matches
        then do
            name <- consume IDENTIFIER "Expect Function name. Use fn instead"
            finishFn (Just name)
        else fn

-- | Parse a lambda function declaration
fn :: Parser Expr
fn = do
    matches <- match [FN]
    if matches
        then finishFn Nothing
        else primary

-- | Shared code for fun and fn parser
finishFn :: Maybe Token -> Parser Expr
finishFn name = do
    _ <- consume LPAR "Expect opening parentheses"
    args <- argParser []
    _ <- consume LBRACE "Expect opening brace"
    body <- bodyParser []
    case name of
      Just n -> return $ Fun n args body
      Nothing -> return $ Fn args body
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
                _ <- consume RPAR "Excpect closing parens"
                return acc
    bodyParser :: [Expr] -> Parser [Expr]
    bodyParser acc = do
        expr <- expression
        matches <- match [RBRACE]
        if matches
            then return (acc ++ [expr])
            else bodyParser (acc ++ [expr])

-- | Parse primary expressions:
--
--   * Identifiers
--   * Groupings
--   * Blocks
--   * and Literals
primary :: Parser Expr
primary = do
   cur <- peek
   case t_type cur of
       IDENTIFIER -> do
           _ <- advance
           return $ Const cur
       LPAR -> do
           _ <- advance
           expr <- expression
           _ <- consume RPAR "Expect closing paren"
           return $ Grouping expr
       LBRACE -> do
           _ <- advance
           block []
       _ ->
           if isLitToken cur
              then do
                  _ <- advance
                  return $ Literal cur
              else throwError $ ParserError ("Cannot parse " ++ show cur) (t_line cur)
  where
    isLitToken cur = t_type cur `elem` [TRUE, FALSE, STRING, NUMBER]

-- | Helper to finish parsing a block
block :: [Expr] -> Parser Expr
block acc = do
    matches <- match [RBRACE]
    if matches
        then return $ Block (reverse acc)
        else do
            expr <- expression
            block (expr:acc)

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
                right <- next
                loop $ Binary left op right
            else return left


{- * Parsing Primitives -}

-- | check if we are at the end
isAtEnd :: Parser Bool
isAtEnd = do
    st <- get
    case tokens st of
        ((Token EOF _ _)):_ -> return True
        _ -> return False

-- | advance the state by one token
advance :: Parser Token
advance = do
    atEnd <- isAtEnd
    if atEnd
        then do
             eof <- peek
             throwError $ ParserError "Unexpected End of Input"  (t_line eof)
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
            _ <- advance
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
        else do
            errT <- peek
            throwError $ ParserError (errMsg ++ " where: " ++ show ttype) (t_line errT)

-- | returns the current token
peek :: Parser Token
peek = do
    st <- get
    case tokens st of
        (x:_) -> return x
        _ -> do
            prev <- previous
            throwError $ ParserError "Out of tokens" (t_line prev)

-- | returns the previous token
previous :: Parser Token
previous = do
    st <- get
    case previousTokens st of
        (x:_) -> return x
        _ -> throwError $ ParserError "No previous token" 0
