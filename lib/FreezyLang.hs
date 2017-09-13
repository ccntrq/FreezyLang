{-|
    A Freezy program might look like this:

    @
    fun plus(a, b,) {
        a + b
    }

    # this is a comment :)

    let a = fn (a,b){a+b}(1,4)
    print a # prints 5

    if plus( 2, 5) > 7
        then "wut"
        else "jay"

    let a = 10
    let name = "peter"

    print name
    print a;
    print name ~ a
    @

   Semicolon might be ommited
   Dangling Commas are allowed
-}

module FreezyLang where

import qualified Data.Map as M

-- |Freezy syntax tokens
data TokenType
    -- |Identifier
    = IDENTIFIER
    -- |Literals
    | NUMBER | STRING
    -- |Booleans
    | TRUE | FALSE
    -- |Conditional branching
    | IF | THEN | ELSE
    -- |Assignment / Function declarations / Lambdas
    | LET | EQUAL | FUN | FN
    -- |Print
    | PRINT
    -- |Math operators
    | PLUS | MINUS | DASH | STAR
    -- |Comparison operatos
    | GR_EQ | LE_EQ | GR | LE
    -- |Equality check operators
    | EQ_EQ | BANG_EQ
    -- |String Concatenation Operator
    | TILDE
    -- |Not operator
    | BANG
    -- |Misc
    | COMMA | SEMICOLON
    | LPAR | RPAR | LBRACE | RBRACE
    -- |Special EOF token
    | EOF
    -- |Throwaway token for whitespace/comments
    | WS
    deriving (Show, Eq)

-- | The full token type with additional information
data Token = Token
    { t_type :: TokenType
    , t_lexeme :: String -- ^ The lexeme for literals
    , t_line :: Int -- ^ the line of the token is tracked for error reporting
    } deriving (Show)

-- |Freezy Expressions
data Expr
    -- |Unary minus and not
    = Unary Token Expr
    -- |Binary Operations
    | Binary Expr Token Expr
    -- |String and Number Literals
    | Literal Token
    -- |Conditional. Both @then@ and @else@ are mandatory
    | IfExpr Expr Expr Expr
    -- |Function Call
    | Call Expr [Expr]
    -- |Lambdas
    | Fn [Token] [Expr]
    -- | Function Declarations evaluate to a callable instance of them self
    | Fun Token [Token] [Expr]
    -- |Constant definition
    | Let Token Expr
    -- |Constant use
    | Const Token
    -- |Groupings
    | Grouping Expr
    -- |evaluates to the printed string
    | Print Expr
    deriving (Show)

-- | A FreezyProgram is just a list of Expressions. This is
--
--   * the output of the main parsing routine
--
--   * the input to the main interpreter
type Program = [Expr]

-- | a simple Environment for the Evaluator.
--
--   * this goes here because we store a closure (Env) in Function Values.
type Env = M.Map String FreezyValue

-- |Freezy Runtime Values
data FreezyValue
    -- |Unbounded Integers
    = Number Integer
    -- |Strings
    | String String
    -- |Boolean
    | Boolean Bool
    -- |Functions
    | Function Env [Token] [Expr]
    deriving (Show)
