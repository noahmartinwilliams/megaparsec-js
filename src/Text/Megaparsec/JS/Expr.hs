module Text.Megaparsec.JS.Expr where

import Control.Monad
import Control.Monad.State
import Control.Monad.Combinators.Expr
import Text.Megaparsec.JS.Ident
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import {-# SOURCE #-} Text.Megaparsec.JS.Func
import {-# SOURCE #-} Text.Megaparsec.JS.JSON
import Text.Megaparsec.JS.Space
import {-# SOURCE #-} Text.Megaparsec.JS.Statem 
import Text.Megaparsec.JS.String
import Text.Megaparsec.JS.Types
import {-# SOURCE #-} Text.Megaparsec.JS.VarDeclaration

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

jsExprBool :: JSParser Expr 
jsExprBool = do
    val <- scn1 (string "true" <|> string "false")
    if val == "true"
    then
        return (BoolExpr True)
    else 
        return (BoolExpr False)

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

mkEqualityExpr :: Expr -> Expr -> Expr
mkEqualityExpr e1 e2 = BinOpExpr e1 e2 EqualityBinOp

mkLogAndExpr :: Expr -> Expr -> Expr
mkLogAndExpr e1 e2 = BinOpExpr e1 e2 LogAndBinOp

parens :: JSParser a -> JSParser a
parens x = do
    scn1 $ between (string "(") (string ")") x

comma :: JSParser ()
comma = do
    void $ scn1 (single ',')
    return ()

funcCallExpr :: JSParser (Expr -> Expr)
funcCallExpr = do
    args <- parens ((scn1 jsExpr) `sepBy` comma)
    return (`FuncCallExpr` args)

newExpr :: (Expr -> Expr)
newExpr = (\x -> NewExpr x)
    
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

jsTernary :: JSParser (Expr -> Expr -> Expr -> Expr)
jsTernary = do
    void $ scn1 (single ':')
    return (\cond -> \thenV -> \elseV -> TernaryExpr cond thenV elseV)

jsExprOp :: JSParser Expr
jsExprOp = do
    let binary name f = InfixL (f <$ symbol lscn1 name)
        postfix p = Postfix p
        prefix p = Prefix p
        table = [ [postfix funcCallExpr], 
            [Prefix (newExpr <$ symbol lscn1 "new")] , 
            [binary "*" mkMulExpr, binary "/" mkDivExpr], 
            [binary "+" mkAddExpr, binary "-" mkSubExpr], 
            [binary "==" mkEqualityExpr], 
            [binary "&&" mkLogAndExpr],
            [binary "=" mkAssignExpr], 
            [TernR (jsTernary <$ scn1 (single '?'))], 
            [binary "." mkMemAccExpr] ]
        terms = (jsExprBool <|> jsAnonFuncExpr <|> (parens jsExprOp) <|> jsStringLit <|> jsExprInt <|> jsExprVar)
    makeExprParser terms table <?> "expression"

jsExpr :: JSParser Expr 
jsExpr = do
    e <- (try jsJSON <|> try jsExprOp <|> try jsAnonFuncExpr <|> try jsStringLit <|> try jsExprInt <|> try jsExprVar )
    return e
