{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Data.Either
import Data.Map
import Expr
import Statem
import Text.Megaparsec
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration
import VarDeclares

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
    test03 -- VarDeclare
    test04
    test05 -- Expr
    test06
    test07
    test08 -- Statem
    test09 -- Statem
    test10 -- Statem
    test11 -- Statem
    test12
    test13
    test14
    test15
    test16
    test17
    test18 -- Expr
    test19
    test20 -- VarDeclare
    test21 -- VarDeclare
    test22 -- Expr
    test23 -- VarDeclares
    test24 -- Statem
    test25 -- Expr
