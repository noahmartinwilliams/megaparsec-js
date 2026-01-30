module Text.Megaparsec.JS.Expr where

import Control.Monad
import Control.Monad.State
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec.JS.Ident
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.String
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration
import {-# SOURCE #-} Text.Megaparsec.JS.Statem 
import {-# SOURCE #-} Text.Megaparsec.JS.Func

jsExprVar :: JSParser Expr
jsExprVar = do
    stateVar <- get
    name <- scn1 jsIdent
    let varLookedUp = lookupVar name stateVar
    return (VarExpr varLookedUp)

jsExprInt :: JSParser Expr
jsExprInt = do
    integer <- scn1 (some digitChar)
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
    void $ scn1 (single ',')
    return ()

funcCallExpr :: JSParser (Expr -> Expr)
funcCallExpr = do
    args <- parens ((scn1 jsExpr) `sepBy` comma)
    return (`FuncCallExpr` args)

jsAnonFuncExpr1 :: JSParser Expr 
jsAnonFuncExpr1 = do
    void $ scn1 (string "function")
    notFollowedBy jsIdent
    void $ scn1 (single '(')
    args <- scn1 (jsArg1)
    void $ scn1 (single ')')
    statems <- jsBlockStatem
    return (AnonFuncExpr args statems)

jsAnonFuncExpr2 :: JSParser Expr
jsAnonFuncExpr2 = do
    void $ scn1 (string "function")
    notFollowedBy jsIdent
    void $ scn1 (single '(')
    void $ scn1 (single ')')
    statems <- scn1 jsBlockStatem
    return (AnonFuncExpr [] statems)


jsAnonFuncExpr :: JSParser Expr
jsAnonFuncExpr = (try jsAnonFuncExpr1 <|> try jsAnonFuncExpr2 )

jsExprOp :: JSParser Expr
jsExprOp = do
    let binary name f = InfixL (f <$ symbol lscn1 name)
        postfix p = Postfix p
        table = [ [postfix funcCallExpr ], [binary "=" mkAssignExpr], [binary "." mkMemAccExpr], [ binary "*" mkMulExpr, binary "/" mkDivExpr], [ binary "+" mkAddExpr, binary "-" mkSubExpr]]
        terms = (jsAnonFuncExpr <|> (parens jsExprOp) <|> jsStringLit <|> jsExprInt <|> jsExprVar)
    makeExprParser terms table <?> "expression"

jsExpr :: JSParser Expr 
jsExpr = do
    e <- (try jsExprOp <|> try jsAnonFuncExpr <|> try jsStringLit <|> try jsExprInt <|> try jsExprVar)
    return e
