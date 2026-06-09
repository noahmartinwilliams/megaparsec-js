module Text.Megaparsec.JS.Regexp where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types

jsRegexpExpr :: JSParser Expr
jsRegexpExpr = do
    str <- scn1 (between (string "/") (string "/") (many character))
    void $ single 'i'
    return (StringLitExpr (str ++ "i")) where
        
        character :: JSParser Char
        character = try escapedChar <|> normalChar

        escapedChar :: JSParser Char 
        escapedChar = char '\\' >> oneOf "\\/sd"

        normalChar :: JSParser Char
        normalChar = satisfy (`notElem` "/\\")
