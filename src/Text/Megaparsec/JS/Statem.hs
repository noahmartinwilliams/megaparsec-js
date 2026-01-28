module Text.Megaparsec.JS.Statem where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types 
import Data.Text as T
import Control.Monad
import Control.Monad.State
import Text.Megaparsec.JS.Expr

jsReturnStatem1 :: JSParser Statem
jsReturnStatem1 = do
    void $ scn1 (string "return")
    e <- scn (jsExpr )
    void $ scn (lookAhead (single '}'))
    return (ReturnStatem e)

jsReturnStatem2 :: JSParser Statem
jsReturnStatem2 = do
    void $ scn1 (string "return")
    e <- scn (jsExpr )
    void $ scn (single ';')
    return (ReturnStatem e)

jsReturnStatem :: JSParser Statem
jsReturnStatem = try (jsReturnStatem2 <|> jsReturnStatem1 )

jsWhileStatem :: JSParser Statem
jsWhileStatem = do
    void $ scn (string "while")
    void $ scn (single '(')
    e <- scn (jsExpr)
    void $ scn (single ')')
    s <- scn (jsStatem )
    return (WhileStatem e s)

jsIfStatem :: JSParser Statem
jsIfStatem = do
    void $ scn (string "if")
    void $ scn (single '(')
    e <- scn (jsExpr)
    void $ scn (single ')' )
    s <- scn (jsStatem)
    return (IfStatem e s)

jsBlockStatem = do
    void $ scn (single '{')
    pstate@(ParserState { scopePath = spath, scopeLevel = slevel, scopePos = spos}) <- get
    put (pstate { scopePath = (spos : spath), scopeLevel = (slevel + 1), scopePos = (spos + 1)})
    ss <- scn (many jsStatem)
    let folded = Prelude.foldr (BlockStatem) EmptyStatem ss
    void $ scn (single '}')
    put (pstate { scopePath = spath, scopeLevel = slevel})
    return folded

jsStatems = do
    ss <- some (jsStatem)
    return (Prelude.foldr BlockStatem EmptyStatem ss)

jsStatem = (jsIfStatem <|> jsBlockStatem <|> jsWhileStatem <|> jsReturnStatem)
