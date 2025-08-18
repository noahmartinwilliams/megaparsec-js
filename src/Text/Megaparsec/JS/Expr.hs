module Text.Megaparsec.JS.Expr where

import Text.Megaparsec.JS.Ident
import Text.Megaparsec
import Text.Megaparsec.JS.Space as S
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

jsExprVar :: Parser Expr
jsExprVar = do
    stateVar <- get
    name <- jsIdent
    let varLookedUp = lookupVar name stateVar
    return (VarExpr varLookedUp)

jsExprInt :: Parser Expr
jsExprInt = do
    integer <- S.lexeme (some digitChar)
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

parens :: Parser a -> Parser a
parens = between (string (T.pack "(")) (string (T.pack ")"))

funcCallExpr :: Parser (Expr -> Expr)
funcCallExpr = do
    args <- parens (jsExpr `sepBy` (S.lexeme (string (T.pack ","))))
    return (`FuncCallExpr` args)

jsAnonFuncExpr :: Parser Expr 
jsAnonFuncExpr = do
    void $ S.lexeme (string (T.pack "function"))
    notFollowedBy jsIdent
    void $ S.lexeme (single '(')
    args <- S.lexeme (jsArg1)
    void $ S.lexeme (single ')')
    statems <- jsBlockStatem
    return (AnonFuncExpr args statems)

jsExprOp :: Parser Expr
jsExprOp = do
    let binary name f = InfixL (f <$ symbol Text.Megaparsec.Char.space  (T.pack name))
        postfix p = Postfix p
        table = [ [postfix funcCallExpr ], [binary "=" mkAssignExpr], [binary "." mkMemAccExpr], [ binary "*" mkMulExpr, binary "/" mkDivExpr], [ binary "+" mkAddExpr, binary "-" mkSubExpr]]
        term = (jsExprInt <|> jsExprVar <|> parens jsExpr) <?> "term"
    makeExprParser term table <?> "expression"

jsExpr :: Parser Expr 
jsExpr = do
    e <- try (jsAnonFuncExpr <|> jsExprOp)
    return e
