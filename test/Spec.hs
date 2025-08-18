{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Data.Text as T
import Data.Either
import Data.Map
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
        result = runParserT varDeclarationSimple "" (T.pack input)
        (result', _) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
    if isRight result'
    then do
        let (Right result'') = result'
        let (VarDeclareStatem [(LocalVar { varName = vname }, Nothing)]) = result''
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

test04 :: IO ()
test04 = do
    let input = "let v;"
        result = runParserT varDeclarationSimple "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
        input2 = "v + 1"
        result2 = runParserT jsExpr "" (T.pack input2)
        (result2', _) = runState result2 newState
    if isRight result2'
    then do
        let (Right result2'') = result2'
        let (BinOpExpr _ _ bop ) = result2''
        if bop == AddBinOp
        then
            putStrLn ("Test 04 succeeded.")
        else
            putStrLn ("Test 04 failed.")
    else
        let (Left err) = result2' in putStrLn (errorBundlePretty err)

test05 :: IO ()
test05 = do
    let input = "let v;"
        result = runParserT varDeclarationSimple "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
        input2 = "v . mem"
        result2 = runParserT jsExpr "" (T.pack input2)
        (result2', _) = runState result2 newState
    if isRight result2'
    then do
        let (Right result2'') = result2'
        let (BinOpExpr _ _ op) = result2''
        if op == MemAccBinOp
        then
            putStrLn ("Test 05 succeeded.")
        else
            putStrLn ("Test 05 failed.")
    else
        let (Left err) = result2' in putStrLn (errorBundlePretty err)

test06 :: IO ()
test06 = do
    let input = "let v;"
        result = runParserT varDeclarationSimple "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
        input2 = "v = v"
        result2 = runParserT jsExpr "" (T.pack input2)
        (result2', _) = runState result2 newState
    if isRight result2'
    then do
        let (Right result2'') = result2'
        let (BinOpExpr _ _ op) = result2''
        if op == AssignBinOp
        then
            putStrLn ("Test 06 succeeded.")
        else
            putStrLn ("Test 06 failed.")
    else
        let (Left err) = result2' in putStrLn (errorBundlePretty err)

test07 :: IO ()
test07 = do
    let input = "let v;"
        result = runParserT varDeclarationSimple "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
        input2 = "v = v * v + v"
        result2 = runParserT jsExpr "" (T.pack input2)
        (result2', _) = runState result2 newState
    if isRight result2'
    then do
        let (Right result2'') = result2'
        let (BinOpExpr _ _ op2) = result2''
        if op2 == AddBinOp
        then
            putStrLn ("Test 07 succeeded.")
        else
            putStrLn ("Test 07 failed. Got: \"" ++ (show op2) ++ "\".")
    else
        let (Left err) = result2' in putStrLn (errorBundlePretty err)

test08 :: IO ()
test08 = do
    let input = "return 1;"
        result = runParserT jsReturnStatem "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
    if isRight result'
    then do
        let (Right result2) = result'
        if (ReturnStatem (IntExpr 1)) == result2
        then
            putStrLn ("Test 08 succeeded.")
        else
            putStrLn ("Test 08 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test09 :: IO ()
test09 = do
    let input = "while (1) return 1;"
        result = runParserT jsStatem "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
    if isRight result'
    then do
        let (Right result2) = result'
        if (WhileStatem (IntExpr 1) (ReturnStatem (IntExpr 1))) == result2
        then
            putStrLn ("Test 09 succeeded.")
        else
            putStrLn ("Test 09 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test10 :: IO ()
test10 = do
    let input = "while (1) {return 1; }"
        result = runParserT jsStatem "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
    if isRight result'
    then do
        let (Right result2) = result'
        if (WhileStatem (IntExpr 1) (BlockStatem (ReturnStatem (IntExpr 1)) EmptyStatem)) == result2
        then
            putStrLn ("Test 10 succeeded.")
        else
            putStrLn ("Test 10 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test11 :: IO ()
test11 = do
    let input = "if (1) { return 0; }"
        result = runParserT jsStatem "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
    if isRight result'
    then do
        let (Right result2) = result'
        if (IfStatem (IntExpr 1) (BlockStatem (ReturnStatem (IntExpr 0)) EmptyStatem)) == result2
        then
            putStrLn ("Test 11 succeeded.")
        else
            putStrLn ("Test 11 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test12 :: IO ()
test12 = do
    let input = "function myFunc(a, b) { return a + b ; }"
        result = runParserT jsFunc "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = (T.pack "") })
    if isRight result'
    then do
        let (Right (Funct fname _ _)) = result'
        if fname == (T.pack "myFunc")
        then
            putStrLn ("Test 12 succeeded.")
        else
            putStrLn ("Test 12 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test13 :: IO ()
test13 = do
    let input = "function myFunc(a, b) { return a + b ; }"
        result = runParserT jsFunc "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = (T.pack "") })
    if isRight result'
    then do
        let (Right (Funct _ vars _)) = result'
        if (vars !! 0) == LocalVar { varPath = [1, 1], varFunctionName = (T.pack "myFunc"), varName = (T.pack "a"), varScopeLevel = 1, varScopePos = 1}
        then
            putStrLn ("Test 13 succeeded.")
        else
            putStrLn ("Test 13 failed. Got : " ++ (show vars))
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test14 :: IO ()
test14 = do
    let input = "func(1, 2)"
        result = runParserT jsExpr "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = (T.pack "") })
    if isRight result'
    then do
        let (Right (FuncCallExpr _ args)) = result'
        if (args !! 0) == (IntExpr 1)
        then
            putStrLn ("Test 14 succeeded.")
        else
            putStrLn ("Test 14 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test15 :: IO ()
test15 = do
    let input = "function f(a) { return a + 1; } var v ;"
        result = runParserT jsDoc "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = (T.pack "") })
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
        result = runParserT jsDoc "" (T.pack input)
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = (T.pack "") })
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
