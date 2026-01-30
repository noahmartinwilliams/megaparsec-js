module Text.Megaparsec.JS.Statem where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types 
import Text.Megaparsec.JS.VarDeclaration
import Data.Text as T
import Control.Monad
import Control.Monad.State
import Text.Megaparsec.JS.Expr

jsReturnStatem1 :: JSParser Statem
jsReturnStatem1 = do
    void $ scn1 (string "return")
    e <- scn1 (jsExpr )
    void $ scn1 (lookAhead (single '}'))
    return (ReturnStatem e)

jsReturnStatem2 :: JSParser Statem
jsReturnStatem2 = do
    void $ scn1 (string "return")
    e <- scn1 (jsExpr )
    void $ scn1 (single ';')
    return (ReturnStatem e)

jsReturnStatem :: JSParser Statem
jsReturnStatem = try (jsReturnStatem2 <|> jsReturnStatem1 )

jsWhileStatem :: JSParser Statem
jsWhileStatem = do
    void $ scn1 (string "while")
    void $ scn1 (single '(')
    e <- scn1 (jsExpr)
    void $ scn1 (single ')')
    s <- scn1 (jsStatem )
    return (WhileStatem e s)

jsExprStatem :: JSParser Statem
jsExprStatem = do
    e <- scn1 jsExpr
    void $ scn1 (single ';')
    return (ExprStatem e)

jsIfStatem :: JSParser Statem
jsIfStatem = do
    void $ scn1 (string "if")
    void $ scn1 (single '(')
    e <- scn1 (jsExpr)
    void $ scn1 (single ')' )
    s <- scn1 (jsStatem)
    return (IfStatem e s)

jsBlockStatem = do
    void $ scn1 (single '{')
    pstate@(ParserState { scopePath = spath, scopeLevel = slevel, scopePos = spos}) <- get
    put (pstate { scopePath = (spos : spath), scopeLevel = (slevel + 1), scopePos = (spos + 1)})
    ss <- scn1 (many jsStatem)
    let folded = Prelude.foldr (BlockStatem) EmptyStatem ss
    void $ scn1 (single '}')
    put (pstate { scopePath = spath, scopeLevel = slevel})
    return folded

jsStatems = do
    ss <- some (jsStatem)
    return (Prelude.foldr BlockStatem EmptyStatem ss)

jsVarDeclareStatem :: JSParser Statem
jsVarDeclareStatem = jsVarDeclarationSimple

jsStatem = (try jsVarDeclareStatem <|> try jsIfStatem <|> try jsBlockStatem <|> try jsWhileStatem <|> try jsReturnStatem <|> try jsExprStatem)
