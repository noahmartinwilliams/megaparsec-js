{-# LANGUAGE OverloadedStrings #-}
module Text.Megaparsec.JS.VarDeclaration where

import Text.Megaparsec
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types
import Control.Monad.State
import Text.Megaparsec.JS.Ident
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Map

lookupVar vname state@(ParserState { scopePath = sp, variables = vars, scopeLevel = sl, scopePos = spos, currentFuncName = cfn}) = do
    let lookedup = Data.Map.lookup vname vars
    if isNothing lookedup 
    then
        (UnknownVar vname) -- For variables that must have been imported from a library that we didn't get to see.
    else
        let (Just lookedup') = lookedup in getVarIntern lookedup' sp sl spos cfn where

            getVarIntern :: [Variable] -> [Int] -> Int -> Int -> String -> Variable
            getVarIntern vlist spath slevel sposn cfn = do
                let vlist' = Prelude.filter (\x -> isInFunction cfn x ) vlist
                    [vlist''] = Prelude.filter (\x -> isInScope spath x) vlist'
                vlist''


            isInScope :: [Int] -> Variable -> Bool
            isInScope slist (LocalVar { varPath = vpath }) = slist == vpath
            isInScope _ (GlobalVar _) = True
            isInScope _ (UnknownVar _) = True

            isInFunction :: String -> Variable -> Bool
            isInFunction _ (UnknownVar _) = True
            isInFunction _ (GlobalVar _) = True
            isInFunction vfn' (LocalVar { varFunctionName = vfn }) | vfn == vfn' = True
            isInFunction _ _ = False

jsVarDeclarationAssign1 :: JSParser (String, Maybe Expr)
jsVarDeclarationAssign1 = do
    fullName <- scn1 jsIdent
    void $ scn1 (single '=')
    e <- scn1 jsExpr
    return (fullName, Just e)

jsVarDeclarationAssign0 :: JSParser (String, Maybe Expr)
jsVarDeclarationAssign0 = do
    fullName <- scn1 jsIdent
    return (fullName, Nothing)
    
jsVarDeclarationAssignMany :: JSParser [(String, Maybe Expr)]
jsVarDeclarationAssignMany = do
    sepBy (try jsVarDeclarationAssign1 <|> try jsVarDeclarationAssign0) (scn1 (void $ single ','))


jsVarDeclarationSimple = do
    pstate@(ParserState { scopePath = sp, variables = vars, scopeLevel = slevel, scopePos = spos, currentFuncName = curFName}) <- get

    declType <- scn1 (string "let" <|> string "var")
    variables <- jsVarDeclarationAssignMany
    
    void $ scn1 (single ';')

    case declType of
        "let" -> do
            let lvars = Prelude.map (\(fullName, _) -> (LocalVar { varPath = sp, varFunctionName = curFName, varName = fullName, varScopeLevel = slevel, varScopePos = spos})) variables
                lvars' = Prelude.zipWith (\x -> \(_, expr) -> (x, expr)) lvars variables
            put (pstate { variables = (insertVars vars lvars ) })
            return (VarDeclareStatem lvars')

        "var" -> do
            let lvars = Prelude.map (\(fullName, _) -> (LocalVar { varFunctionName = curFName, varName = fullName, varScopeLevel = 1, varScopePos = spos, varPath = sp })) variables
                lvars' = Prelude.zipWith (\x -> \(_, expr) -> (x, expr)) lvars variables
            put (pstate { variables = (insertVars vars lvars) })
            return (VarDeclareStatem lvars') 

insertVar m variable@(LocalVar { varName = vname }) = do
    let lu = Data.Map.lookup vname m 
    if isNothing lu
    then
        insert vname [variable] m
    else
        let (Just lu') = lu in insert vname (variable : lu') m

insertVars :: Map String [Variable] -> [Variable] -> Map String [Variable]
insertVars m [] = m
insertVars m (head : tail) = insertVars (insertVar m head) tail
