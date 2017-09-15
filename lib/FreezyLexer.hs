{-| FreezyLexer - A Lexer for FreezyLang -}
module FreezyLexer where

import FreezyLang

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Char  as C

-- | The Lexer monad stack
--
--     * ExceptT for error handling
--
--     * StateT for the lexerstate
--
--     * Identity as the base monad
type Lexer a = ExceptT LexerError (StateT LexerState Identity) a

-- | Unwrapping of the monad stack
runLexer :: LexerState -> Lexer a -> (Either LexerError a, LexerState)
runLexer st l = runIdentity $ runStateT (runExceptT l) st

-- | The State of the Lexer
data LexerState = LexerState { line :: Int } deriving (Show)

-- | initial state constructor
initState :: LexerState
initState = LexerState 1

-- | increments the line in the lexer state
incLine :: Lexer ()
incLine = do
    st <- get
    put (st {line = (line st) + 1 })

-- | query the lexerstate for the current line
queryLine :: Lexer Int
queryLine = do
    st <- get
    return $ line st

-- | Type for lexer errors
--   I'm not sure if this is really required. we can get the line from the
--   state but i will stick with it for now
data LexerError = LexerError
    { reason :: String
    , e_line :: Int
    } deriving (Show)

-- | Tokenize the complete input
lexIt :: String -> Lexer [Token]
lexIt xs = lexIt' [] xs
  where
    lexIt' tokens [] = do
        (eof, _) <- mkToken EOF ""
        return $ reverse (eof:tokens)
    lexIt' tokens xs' = do
        (token, rest) <- lexNext xs'
        case t_type token of
            WS -> lexIt' tokens rest -- skip the whitespace tokens
            _  -> lexIt' (token:tokens) rest

-- | Lex the next token
lexNext :: String -> Lexer (Token, String)
lexNext ('>':'=':xs) = mkToken GR_EQ xs
lexNext ('<':'=':xs) = mkToken LE_EQ xs
lexNext ('=':'=':xs) = mkToken EQ_EQ xs
lexNext ('!':'=':xs) = mkToken BANG_EQ xs
lexNext ('=':xs) = mkToken EQUAL xs
lexNext ('+':xs) = mkToken PLUS xs
lexNext ('-':xs) = mkToken MINUS xs
lexNext ('/':xs) = mkToken DASH xs
lexNext ('*':xs) = mkToken STAR xs
lexNext ('>':xs) = mkToken GR xs
lexNext ('<':xs) = mkToken LE xs
lexNext ('!':xs) = mkToken BANG xs
lexNext ('~':xs) = mkToken TILDE xs
lexNext (',':xs) = mkToken COMMA xs
lexNext (';':xs) = mkToken SEMICOLON xs
lexNext ('(':xs) = mkToken LPAR xs
lexNext (')':xs) = mkToken RPAR xs
lexNext ('{':xs) = mkToken LBRACE xs
lexNext ('}':xs) = mkToken RBRACE xs
lexNext (' ':xs) = mkToken WS xs
lexNext ('\n':xs) = do
    incLine
    mkToken WS xs
lexNext ('#':xs) = lexComment "" xs

lexNext ('"':xs) = lexStr "" xs
lexNext (x:xs)
    | isDigit x = lexNum [x] xs
    | isAlpha x = lexIdent [x] xs
    | otherwise = mkError $ "Unexpected input: " ++ show x
lexNext []     = mkError "Ups. This shouldn't happen"

-- | Lex a single line comment
lexComment :: String -> String -> Lexer (Token, String)
lexComment acc [] = mkLitToken WS (reverse acc) []
lexComment acc ('\n':xs) = do
    incLine
    mkLitToken WS (reverse acc) xs
lexComment acc (x:xs) = lexComment (x:acc) xs


-- | Lex an Integer number
lexNum :: String -> String -> Lexer (Token, String)
lexNum acc [] = mkLitToken NUMBER (reverse acc) []
lexNum acc source@(x:xs)
    | isDigit x = lexNum (x:acc) xs
    | otherwise = mkLitToken NUMBER (reverse acc) source

-- | Lex an identifier or reserved keyword
lexIdent :: String -> String -> Lexer (Token, String)
lexIdent acc [] = mkReservedOrIdentToken (reverse acc) []
lexIdent acc source@(x:xs)
    | isAlphaNum x = lexIdent (x:acc) xs
    | otherwise = mkReservedOrIdentToken (reverse acc) source

-- | Lex a string
lexStr :: String -> String -> Lexer (Token, String)
lexStr _ [] = mkError "Expext String terminator"
lexStr acc ('"':xs) = mkLitToken STRING (reverse acc) xs
lexStr acc ('\n':xs) = do
    incLine
    lexStr ('\n':acc) xs
lexStr acc (x:xs) = lexStr (x:acc) xs

{- * Helper Functions -}

-- | convenience helper for literal tokens
mkLitToken :: TokenType -> String -> String -> Lexer (Token, String)
mkLitToken ttype lexeme rest = do
    curLine <- queryLine
    return $ (Token ttype lexeme curLine, rest)

-- | convenience helper for tokens without an interesting lexeme
mkToken :: TokenType -> String -> Lexer (Token, String)
mkToken ttype rest = mkLitToken ttype [] rest

-- | convenience helper that maps the given lexeme to either a reserved
--   keyword or an identifier
mkReservedOrIdentToken :: String -> String -> Lexer (Token, String)
mkReservedOrIdentToken lexeme xs
  | lexeme == "if" = mkToken IF xs
  | lexeme == "then" = mkToken THEN xs
  | lexeme == "else" = mkToken ELSE xs
  | lexeme == "fun" = mkToken FUN xs
  | lexeme == "fn" = mkToken FN xs
  | lexeme == "let" = mkToken LET xs
  | lexeme == "print" = mkToken PRINT xs
  | lexeme == "true" = mkToken TRUE xs
  | lexeme == "false" = mkToken FALSE xs
  | otherwise = mkLitToken IDENTIFIER lexeme xs


-- | convenience helper to create an error on the current line
mkError :: String -> Lexer a
mkError msg = do
    curLine <- queryLine
    throwError $ LexerError msg curLine

{- * Char Helpers -}

-- | simple wrapper around Data.Char.isDigit
isDigit :: Char -> Bool
isDigit = C.isDigit

-- | Data.Char.isAlpa or an underscore
isAlpha :: Char -> Bool
isAlpha c = C.isAlpha c || c == '_'

-- | isDigit or isAlpha
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c
