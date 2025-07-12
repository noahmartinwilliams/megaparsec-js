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


varDeclaration :: Parser [Variable]
varDeclaration = do
    pstate@(ParserState { scopeLevel = slevel, scopePos = spos, currentFuncName = curFName}) <- get

    declType <- S.lexeme (string "let" <|> string "var")
    varFirstLetter <- S.lexeme (upperChar <|> lowerChar)
    varName <- S.lexeme (many alphaNumChar)
    let fullName = T.pack ([varFirstLetter] ++ varName)
    void $ S.lexeme (single ';')
    case declType of
        "let" -> return [(LocalVar { varFunctionName = curFName, varName = fullName, varScopeLevel = slevel, varScopePos = spos})]
        "var" -> return [(LocalVar { varFunctionName = curFName, varName = fullName, varScopeLevel = 1, varScopePos = spos})]
