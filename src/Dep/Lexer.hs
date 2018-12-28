-- | A module that provides utility methods for the lexers used in the program.
module Dep.Lexer (
    TokenType(..),premarkup,
    showAnsiToken
    ) where

-- | The different types of tokens. This is used to syntax-highlight output code.
data TokenType = Keyword -- ^ The token is a keyword: a predefined string.
     | Literal -- ^ The token is a literal: a constant value.
     | Identifier -- ^ The token is an identifier: a string used to reference a variable.
     | Delimiter -- ^ The token is delimiter: an operand, brackets, etc.
     | Other -- ^ The token is something else.
     deriving (Show,Eq,Enum,Bounded)

-- | Map the given tokentype to a string containing ANSI terminal escape characters to give the token the appropriate syntax highlighting.
premarkup :: TokenType -- ^ The given type of token to calculate its syntax highlighting for.
    -> String -- ^ The resulting string that contains the appropriate ANSI terminal escape sequence.
premarkup Keyword = "\x1b[32;1m"
premarkup Literal = "\x1b[31m"
premarkup Identifier = "\x1b[1;31m"
premarkup Delimiter = "\x1b[34m"
premarkup _ = ""

-- | A function that for a given mapping to its type presents its show with the correct ANSI terminal escape syntax highlighting.
showAnsiToken :: Show a => (a -> TokenType) -- ^ The given mapping from the token to its token type.
    -> a -- ^ The given element to show with its correct syntax highlighting.
    -> String -- ^ The string containing both the show and is correctly highlighted on an ANSI terminal.
showAnsiToken tokType x = premarkup (tokType x)++show x++"\x1b[0m"
