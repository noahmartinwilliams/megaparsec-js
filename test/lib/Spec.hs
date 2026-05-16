{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Data.Either
import Data.Map
import Expr
import Text.Megaparsec
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration

test01 :: IO ()
test01 = do
    let input = "let v;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', _) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    if isRight result'
    then do
        let (Right result'') = result'
        let (VarDeclareStatem [(LocalVar { varName = vname }, Nothing)]) = result''
        if vname == "v"
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
        result = runParserT jsVarDeclarationSimple "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
        input2 = "v"
        result2 = runParserT jsExprVar "" input2
        (result2', _) = runState result2 newState
    if isRight result2'
    then do
        let (Right result2'') = result2'
        let (VarExpr (LocalVar { varName = vname })) = result2''
        if vname == "v"
        then
            putStrLn ("Test 03 succeeded.")
        else
            putStrLn ("Test 03 failed.")
    else
        let (Left err) = result2' in putStrLn (errorBundlePretty err)

test08 :: IO ()
test08 = do
    let input = "return 1;"
        result = runParserT jsReturnStatem "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    if isRight result'
    then do
        let (Right result2) = result'
        if (ReturnStatem (Just (IntExpr 1))) == result2
        then
            putStrLn ("Test 08 succeeded.")
        else
            putStrLn ("Test 08 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test09 :: IO ()
test09 = do
    let input = "while (1) return 1;"
        result = runParserT jsStatem "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    if isRight result'
    then do
        let (Right result2) = result'
        if (WhileStatem (IntExpr 1) (ReturnStatem (Just (IntExpr 1)))) == result2
        then
            putStrLn ("Test 09 succeeded.")
        else
            putStrLn ("Test 09 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test10 :: IO ()
test10 = do
    let input = "while (1) {return 1; }"
        result = runParserT jsStatem "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    if isRight result'
    then do
        let (Right result2) = result'
        if (WhileStatem (IntExpr 1) (BlockStatem (ReturnStatem (Just (IntExpr 1))) EmptyStatem)) == result2
        then
            putStrLn ("Test 10 succeeded.")
        else
            putStrLn ("Test 10 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test11 :: IO ()
test11 = do
    let input = "if (1) { return 0; }"
        result = runParserT jsStatem "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    if isRight result'
    then do
        let (Right result2) = result'
        if (IfStatem (IntExpr 1) (BlockStatem (ReturnStatem (Just (IntExpr 0))) EmptyStatem)) == result2
        then
            putStrLn ("Test 11 succeeded.")
        else
            putStrLn ("Test 11 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test12 :: IO ()
test12 = do
    let input = "function myFunc(a, b) { return a + b ; }"
        result = runParserT jsFunc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    if isRight result'
    then do
        let (Right (Funct fname _ _)) = result'
        if fname == "myFunc"
        then
            putStrLn ("Test 12 succeeded.")
        else
            putStrLn ("Test 12 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test13 :: IO ()
test13 = do
    let input = "function myFunc(a, b) { return a + b ; }"
        result = runParserT jsFunc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    if isRight result'
    then do
        let (Right (Funct _ vars _)) = result'
        if (vars !! 0) == LocalVar { varPath = [1, 1], varFunctionName = "myFunc", varName = "a", varScopeLevel = 1, varScopePos = 1}
        then
            putStrLn ("Test 13 succeeded.")
        else
            putStrLn ("Test 13 failed. Got : " ++ (show vars))
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)


test15 :: IO ()
test15 = do
    let input = "function f(a) { return a + 1; } var v ;"
        result = runParserT jsDoc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    if isRight result'
    then do
        let (Right ((Doc syms), _)) = result'
        if (Prelude.length syms) == 2
        then
            putStrLn ("Test 15 succeeded.")
        else
            putStrLn ("Test 15 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test16 :: IO ()
test16 = do
    let input = "function f(a) { return function(b) { return a + b; } ; }  var v ;"
        result = runParserT jsDoc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    if isRight result'
    then do
        let (Right ((Doc syms), _)) = result'
        if (Prelude.length syms) == 2
        then
            putStrLn ("Test 16 succeeded.")
        else
            putStrLn ("Test 16 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test17 :: IO ()
test17 = do
    let input = "return true ? 2 : 3;"
        result = runParserT jsStatem "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "foo" })
    if isRight result'
    then do
        let (Right (expr)) = result'
        if expr == ReturnStatem (Just (TernaryExpr (BoolExpr True ) (IntExpr 2) (IntExpr 3)))
        then
            putStrLn ("Test 17 succeeded.")
        else
            putStrLn ("Test 17 failed. Got: \"" ++ (show expr) ++ "\".")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
    test02
    test03
    test04
    test05
    test06
    test07
    test08
    test09
    test10
    test11
    test12
    test13
    test14
    test15
    test16
    test17
    test18
