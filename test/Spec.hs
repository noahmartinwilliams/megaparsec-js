{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Megaparsec
import Data.Text as T
import Data.Either
import Data.Map
import Text.Megaparsec.JS.VarDeclaration
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.Expr
import Control.Monad.State

test01 :: IO ()
test01 = do
    let input = "let v;"
        result = runParserT varDeclarationSimple "" (T.pack input)
        (result', _) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
    if isRight result'
    then do
        let (Right result'') = result'
        let (VarDeclare [(LocalVar { varName = vname }, Nothing)]) = result''
        if vname == (T.pack "v")
        then
            putStrLn ("Test 01 succeeded.")
        else
            putStrLn ("Test 01 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test02 :: IO ()
test02 = do
    let v1Var = LocalVar { varPath = [0, 1, 2], varFunctionName = "ultrakill", varName = "v1", varScopeLevel = 2, varScopePos = 0 }
        v2Var = LocalVar { varPath = [0, 1, 2], varFunctionName = "ultrakill", varName = "v2", varScopeLevel = 2, varScopePos = 0 }
        allVars = Data.Map.fromList [("v1", [v1Var]), ("v2", [v2Var])]
        parserState = ParserState { scopePath = [0, 1, 2], variables = allVars, scopeLevel = 2, scopePos = 0, currentFuncName = "ultrakill" }
        var = lookupVar "v1" parserState
    if var /= v1Var
    then
        putStrLn ("Test 02 failed. Got \"" ++ (show var) ++ "\".")
    else
        putStrLn ("Test 02 succeeded.")

test03 :: IO ()
test03 = do
    let input = "let v;"
        result = runParserT varDeclarationSimple "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
        input2 = "v"
        result2 = runParserT jsExprVar "" (T.pack input2)
        (result2', _) = runState result2 newState
    if isRight result2'
    then do
        let (Right result2'') = result2'
        let (VarExpr (LocalVar { varName = vname })) = result2''
        if vname == (T.pack "v")
        then
            putStrLn ("Test 03 succeeded.")
        else
            putStrLn ("Test 03 failed.")
    else
        let (Left err) = result2' in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
    test02
    test03
