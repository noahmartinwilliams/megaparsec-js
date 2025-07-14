module Text.Megaparsec.JS.Expr where

import Text.Megaparsec
import Text.Megaparsec.JS.Space as S
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration
import Data.Text as T
import Control.Monad.State

jsExprVar :: Parser Expr
jsExprVar = do
    stateVar <- get
    firstChar <- S.lexeme letterChar
    restOfName <- many alphaNumChar
    let name = [firstChar] ++ restOfName
        varLookedUp = lookupVar (T.pack name) stateVar
    return (VarExpr varLookedUp)

jsExprInt :: Parser Expr
jsExprInt = do
    integer <- some digitChar
    return (IntExpr (read integer :: Int))

binOp :: Parser BinOp
binOp = do
    op <- S.lexeme (single '+' <|> single '-')
    case op of
        '+' -> return AddBinOp
        '-' -> return SubBinOp

jsExpr :: Parser Expr
jsExpr = (jsExprInt <|> jsExprVar)

jsExprBinOp :: Parser Expr
jsExprBinOp = do
    e1 <- jsExpr
    bop <- binOp
    e2 <- jsExpr 
    return (BinOpExpr e1 e2 bop)
