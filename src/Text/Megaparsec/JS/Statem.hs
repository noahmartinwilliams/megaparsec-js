module Text.Megaparsec.JS.Statem where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Types 
import Data.Text as T
import Control.Monad
import Control.Monad.State
import Text.Megaparsec.JS.Expr

jsReturnStatem1 :: JSParser Statem
jsReturnStatem1 = do
    void $ hspace
    void $ (string "return")
    void $ hspace1
    e <- (jsExpr )
    void $ hspace
    void $ lookAhead (single '}')
    void $ hspace
    return (ReturnStatem e)

jsReturnStatem2 :: JSParser Statem
jsReturnStatem2 = do
    void $ hspace
    void $ (string "return")
    void $ hspace1
    e <- (jsExpr )
    void $ hspace
    void $ (single ';')
    void $ hspace
    return (ReturnStatem e)

jsReturnStatem :: JSParser Statem
jsReturnStatem = try (jsReturnStatem2 <|> jsReturnStatem1 )

jsWhileStatem :: JSParser Statem
jsWhileStatem = do
    void $ hspace
    void $ (string "while")
    void $ hspace
    void $ (single '(')
    void $ hspace
    e <- (jsExpr)
    void $ hspace
    void $ (single ')')
    void $ hspace
    s <- (jsStatem )
    void $ hspace
    return (WhileStatem e s)

jsIfStatem :: JSParser Statem
jsIfStatem = do
    void $ hspace
    void $ (string "if")
    void $ hspace
    void $ (single '(')
    void $ hspace
    e <- (jsExpr)
    void $ hspace
    void $ (single ')' )
    void $ hspace
    s <- (jsStatem)
    void $ hspace
    return (IfStatem e s)

jsBlockStatem = do
    void $ hspace
    void $ (single '{')
    pstate@(ParserState { scopePath = spath, scopeLevel = slevel, scopePos = spos}) <- get
    put (pstate { scopePath = (spos : spath), scopeLevel = (slevel + 1), scopePos = (spos + 1)})
    ss <- (many jsStatem)
    void $ hspace
    let folded = Prelude.foldr (BlockStatem) EmptyStatem ss
    void $ (single '}')
    void $ hspace
    put (pstate { scopePath = spath, scopeLevel = slevel})
    return folded

jsStatems = do
    ss <- some (jsStatem)
    return (Prelude.foldr BlockStatem EmptyStatem ss)

jsStatem = (jsIfStatem <|> jsBlockStatem <|> jsWhileStatem <|> jsReturnStatem)
