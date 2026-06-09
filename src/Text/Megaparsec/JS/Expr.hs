module Text.Megaparsec.JS.Expr where

import Control.Monad
import Control.Monad.State
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.BinExpr
import {-# SOURCE #-} Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Ident
import {-# SOURCE #-} Text.Megaparsec.JS.JSON
import {-# SOURCE #-} Text.Megaparsec.JS.List
import Text.Megaparsec.JS.Misc
import Text.Megaparsec.JS.Regexp
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

funcCallExpr :: JSParser (Expr -> Expr)
funcCallExpr = do
    args <- scn1 (parens ((scn1 jsExpr) `sepBy` (scn1 comma)))
    return (`FuncCallExpr` args)

newExpr :: (Expr -> Expr)
newExpr = (\x -> NewExpr x)
    
typeofExpr :: (Expr -> Expr)
typeofExpr = (\x -> TypeOfExpr x)

jsAnonFuncExpr1 :: JSParser Expr 
jsAnonFuncExpr1 = do
    void $ scn1 (string "function")
    notFollowedBy jsIdent
    void $ scn1 (single '(')
    args <- scn1 (jsArg1)
    void $ scn1 (single ')')
    statems <- scn1 jsBlockStatem
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
        table = [ [binary "." mkMemAccExpr],
            [postfix funcCallExpr], 
            [Prefix (newExpr <$ symbol lscn1 "new")] , 
            [Prefix (typeofExpr <$ symbol lscn1 "typeof")],
            [binary "*" mkMulExpr, binary "/" mkDivExpr], 
            [binary "+" mkAddExpr, binary "-" mkSubExpr], 
            [binary "<" mkLTExpr, binary ">" mkGTExpr],
            [binary "==" mkEqualityExpr, binary "!==" mkStrictInequalityExpr ], 
            [binary "&&" mkLogAndExpr],
            [binary "||" mkLogOrExpr],
            [binary "=" mkAssignExpr], 
            [TernR (jsTernary <$ scn1 (single '?'))]]
        terms = (jsRegexpExpr <|> jsListLit <|> jsJSON <|> jsExprBool <|> jsAnonFuncExpr <|> (parens jsExprOp) <|> jsExprInt <|> jsExprVar <|> jsStringLit)
    makeExprParser terms table <?> "expression"

jsExpr :: JSParser Expr 
jsExpr = jsExprOp
