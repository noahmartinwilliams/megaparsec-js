module Text.Megaparsec.JS.Statem where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Space as S
import Text.Megaparsec.JS.Types 
import Data.Text as T
import Control.Monad
import Text.Megaparsec.JS.Expr

jsReturnStatem :: Parser Statem
jsReturnStatem = do
    void $ S.lexeme (string (T.pack "return"))
    e <- S.lexeme (jsExpr )
    void $ single ';'
    return (ReturnStatem e)

jsWhileStatem :: Parser Statem
jsWhileStatem = do
    void $ S.lexeme (string (T.pack "while"))
    void $ S.lexeme (single '(')
    e <- S.lexeme (jsExpr)
    void $ S.lexeme (single ')')
    s <- S.lexeme (jsStatem )
    return (WhileStatem e s)

jsStatem :: Parser Statem 
jsStatem = (jsWhileStatem <|> jsReturnStatem)
