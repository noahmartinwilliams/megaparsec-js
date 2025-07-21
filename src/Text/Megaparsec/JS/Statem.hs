module Text.Megaparsec.JS.Statem where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Space as S
import Text.Megaparsec.JS.Types 
import Data.Text as T
import Control.Monad
import Control.Monad.State
import Text.Megaparsec.JS.Expr

jsReturnStatem1 :: Parser Statem
jsReturnStatem1 = do
    void $ S.lexeme (string (T.pack "return"))
    e <- S.lexeme (jsExpr )
    void $ lookAhead (single '}')
    return (ReturnStatem e)

jsReturnStatem2 :: Parser Statem
jsReturnStatem2 = do
    void $ S.lexeme (string (T.pack "return"))
    e <- S.lexeme (jsExpr )
    void $ (single ';')
    return (ReturnStatem e)

jsReturnStatem :: Parser Statem
jsReturnStatem = try (jsReturnStatem2 <|> jsReturnStatem1 )

jsWhileStatem :: Parser Statem
jsWhileStatem = do
    void $ S.lexeme (string (T.pack "while"))
    void $ S.lexeme (single '(')
    e <- S.lexeme (jsExpr)
    void $ S.lexeme (single ')')
    s <- S.lexeme (jsStatem )
    return (WhileStatem e s)

jsBlockStatem :: Parser Statem 
jsBlockStatem = do
    void $ S.lexeme (single '{')
    pstate@(ParserState { scopePath = spath, scopeLevel = slevel, scopePos = spos}) <- get
    put (pstate { scopePath = (spos : spath), scopeLevel = (slevel + 1), scopePos = (spos + 1)})
    ss <- S.lexeme (many (S.lexeme jsStatem))
    let folded = Prelude.foldr (BlockStatem) EmptyStatem ss
    void $ S.lexeme (single '}')
    put (pstate { scopePath = spath, scopeLevel = slevel})
    return folded

jsStatem :: Parser Statem 
jsStatem = (jsBlockStatem <|> jsWhileStatem <|> jsReturnStatem)
