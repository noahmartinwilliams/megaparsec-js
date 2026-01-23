module Text.Megaparsec.JS.Expr where

import Text.Megaparsec.JS.Ident
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration
import Data.Text as T
import Control.Monad
import Control.Monad.State
import Control.Monad.Combinators.Expr
import {-# SOURCE #-} Text.Megaparsec.JS.Statem 
import {-# SOURCE #-} Text.Megaparsec.JS.Func
import Data.Void

jsExprVar :: JSParser Expr
jsExprVar = do
    stateVar <- get
    name <- jsIdent
    let varLookedUp = lookupVar name stateVar
    return (VarExpr varLookedUp)

jsExprInt :: JSParser Expr
jsExprInt = do
    void $ hspace
    integer <- (some digitChar)
    void $ hspace
    return (IntExpr (read integer :: Int))

mkMulExpr :: Expr -> Expr -> Expr
mkMulExpr e1 e2 = BinOpExpr e1 e2 MulBinOp

mkDivExpr :: Expr -> Expr -> Expr
mkDivExpr e1 e2 = BinOpExpr e1 e2 DivBinOp

mkAddExpr :: Expr -> Expr -> Expr
mkAddExpr e1 e2 = BinOpExpr e1 e2 AddBinOp

mkSubExpr :: Expr -> Expr -> Expr
mkSubExpr e1 e2 = BinOpExpr e1 e2 SubBinOp

mkAssignExpr :: Expr -> Expr -> Expr
mkAssignExpr e1 e2 = BinOpExpr e1 e2 AssignBinOp

mkMemAccExpr :: Expr -> Expr -> Expr
mkMemAccExpr e1 e2 = BinOpExpr e1 e2 MemAccBinOp

parens :: JSParser a -> JSParser a
parens = between (string "(") (string ")")

comma :: JSParser ()
comma = do
    void $ single ','
    void $ hspace
    return ()

funcCallExpr :: JSParser (Expr -> Expr)
funcCallExpr = do
    args <- parens (jsExpr `sepBy` comma)
    return (`FuncCallExpr` args)

jsAnonFuncExpr :: JSParser Expr 
jsAnonFuncExpr = do
    void $ hspace
    void $ (string "function")
    void $ hspace1
    notFollowedBy jsIdent
    void $ (single '(')
    void $ hspace
    args <- (jsArg1)
    void $ hspace
    void $ (single ')')
    void $ hspace
    statems <- jsBlockStatem
    void $ hspace
    return (AnonFuncExpr args statems)

jsExprOp :: JSParser Expr
jsExprOp = do
    let binary name f = InfixL (f <$ symbol Text.Megaparsec.Char.space name)
        postfix p = Postfix p
        table = [ [postfix funcCallExpr ], [binary "=" mkAssignExpr], [binary "." mkMemAccExpr], [ binary "*" mkMulExpr, binary "/" mkDivExpr], [ binary "+" mkAddExpr, binary "-" mkSubExpr]]
        term = (jsExprInt <|> jsExprVar <|> parens jsExpr) <?> "term"
    makeExprParser term table <?> "expression"

jsExpr :: JSParser Expr 
jsExpr = do
    e <- try (jsAnonFuncExpr <|> jsExprOp)
    return e
