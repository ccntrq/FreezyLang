{-|
    A Freezy program might look like this:

    @
    fun plus(a, b,) {
        a + b
    }

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

-- |Freezy syntax tokens
data Token
    -- |Identifier
    = IDENTIFIER String
    -- |Literals
    | NUMBER Integer | STRING String
    -- |Booleans
    | TRUE | FALSE
    -- |Conditional branching
    | IF | THEN | ELSE
    -- |Assignment / Function declarations
    | LET | EQ | FUN
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
    | LPAR | LRPAR | LBRACE | RBRACE
    -- |Special EOF token
    | EOF

-- |Freezy Expressions
data Expr
    -- |Unary minus and not
    = Unary Token Expr
    -- |Binary Operations
    | Binary Expr Token Expr
    -- |String and Number Literals
    | Literal Token
    -- |Conditional. Both @then@ and @else@ are mandatory
    | Cond Expr Expr Expr
    -- |Function Call
    | Call Token [Expr]
    -- |Function Declarations evaluate to a callable instance of them self
    | Fun Token [Token] [Expr]
    -- |Constant definition
    | Let Token Expr
    -- |Constant use
    | Const Token
    -- |evaluates to the printed string
    | Print Expr

-- |Freezy Runtime Values
data Value
    -- |Unbounded Integers
    = Number Integer
    -- |Strings
    | String String
    -- |Boolean
    | Boolean Bool
    -- |Functions
    | Function [Token] [Expr]
