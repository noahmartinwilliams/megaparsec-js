{-# LANGUAGE OverloadedStrings #-}
module Text.Megaparsec.JS.VarDeclaration where

import Text.Megaparsec
import Data.Text as T
import Text.Megaparsec.JS.Types
import Control.Monad.State
import Text.Megaparsec.JS.Space as S
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Map

lookupVar :: Text -> ParserState -> Variable
lookupVar vname state@(ParserState { scopePath = sp, variables = vars, scopeLevel = sl, scopePos = spos, currentFuncName = cfn}) = do
    let lookedup = Data.Map.lookup vname vars
    if isNothing lookedup 
    then
        (UnknownVar vname) -- For variables that must have been imported from a library that we didn't get to see.
    else
        let (Just lookedup') = lookedup in getVarIntern lookedup' sp sl spos cfn where

            getVarIntern :: [Variable] -> [Int] -> Int -> Int -> Text -> Variable
            getVarIntern vlist spath slevel sposn cfn = do
                let vlist' = Prelude.filter (\x -> isInFunction cfn x ) vlist
                    [vlist''] = Prelude.filter (\x -> isInScope spath x) vlist'
                vlist''


            isInScope :: [Int] -> Variable -> Bool
            isInScope slist (LocalVar { varPath = vpath }) = slist == vpath
            isInScope _ (GlobalVar _) = True
            isInScope _ (UnknownVar _) = True

            isInFunction :: Text -> Variable -> Bool
            isInFunction _ (UnknownVar _) = True
            isInFunction _ (GlobalVar _) = True
            isInFunction vfn' (LocalVar { varFunctionName = vfn }) | vfn == vfn' = True
            isInFunction _ _ = False

varDeclarationSimple :: Parser Statem
varDeclarationSimple = do
    pstate@(ParserState { scopePath = sp, variables = vars, scopeLevel = slevel, scopePos = spos, currentFuncName = curFName}) <- get

    declType <- S.lexeme (string "let" <|> string "var")
    varFirstLetter <- S.lexeme (upperChar <|> lowerChar)
    varName <- S.lexeme (many alphaNumChar)

    let fullName = T.pack ([varFirstLetter] ++ varName)
    
    void $ S.lexeme (single ';')

    case declType of
        "let" -> do
            let lvar = (LocalVar { varPath = sp, varFunctionName = curFName, varName = fullName, varScopeLevel = slevel, varScopePos = spos})
            put (pstate { variables = (insertVar vars lvar ) })
            return (VarDeclareStatem [(lvar, Nothing)])
        "var" -> do
            let lvar = (LocalVar { varFunctionName = curFName, varName = fullName, varScopeLevel = 1, varScopePos = spos})
            put (pstate { variables = (insertVar vars lvar) })
            return (VarDeclareStatem [(lvar, Nothing)]) 

insertVar :: Map Text [Variable] -> Variable -> Map Text [Variable]
insertVar m variable@(LocalVar { varName = vname }) = do
    let lu = Data.Map.lookup vname m 
    if isNothing lu
    then
        insert vname [variable] m
    else
        let (Just lu') = lu in insert vname (variable : lu') m
