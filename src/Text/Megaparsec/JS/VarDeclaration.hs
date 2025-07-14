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


varDeclarationSimple :: Parser Statement
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
            return (VarDeclare [(lvar, Nothing)])
        "var" -> do
            let lvar = (LocalVar { varFunctionName = curFName, varName = fullName, varScopeLevel = 1, varScopePos = spos})
            put (pstate { variables = (insertVar vars lvar) })
            return (VarDeclare [(lvar, Nothing)]) 

insertVar :: Map Text [Variable] -> Variable -> Map Text [Variable]
insertVar m variable@(LocalVar { varName = vname }) = do
    let lu = Data.Map.lookup vname m 
    if isNothing lu
    then
        insert vname [variable] m
    else
        let (Just lu') = lu in insert vname (variable : lu') m
