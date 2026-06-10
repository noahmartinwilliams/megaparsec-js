module Text.Megaparsec.JS.Statem where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import {-# SOURCE #-} Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Ident
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
    e <- scn1 (optional jsExpr )
    void $ scn1 (lookAhead (single '}'))
    return (ReturnStatem e)

jsReturnStatem2 :: JSParser Statem
jsReturnStatem2 = do
    void $ scn1 (string "return")
    e <- scn1 (optional jsExpr )
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
    ss <- scn1 jsStatems
    void $ scn1 (single '}')
    put (pstate { scopePath = spath, scopeLevel = slevel})
    return ss

jsFuncStatem :: JSParser Statem
jsFuncStatem = do
    void $ scn1 (string "function")
    name <- scn1 jsIdent
    void $ scn1 (single '(')
    args <- scn1 (optional jsArgList)
    void $ scn1 (single ')')
    st <- scn1 jsBlockStatem
    void $ scn1 (single ';')
    case args of
        Nothing -> return (FuncStatem (Funct name [] st))
        (Just a) -> return (FuncStatem (Funct name a st))

jsStatems = do
    ss <- some (scn1 jsStatem)
    return (Prelude.foldr BlockStatem EmptyStatem ss)

jsVarDeclareStatem :: JSParser Statem
jsVarDeclareStatem = jsVarDeclarationSimple

jsStatem1 :: JSParser Statem
jsStatem1 = (try jsFuncStatem <|> try jsVarDeclareStatem <|> try jsIfStatem <|> try jsBlockStatem <|> try jsWhileStatem <|> try jsReturnStatem <|> try jsExprStatem )

jsStatem2 :: JSParser Statem
jsStatem2 = do
    j <- scn1 jsStatem1
    void $ scn1 (single ';')
    return j

jsStatem = (try jsStatem2 <|> try jsStatem1)
