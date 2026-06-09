{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Data.Either
import Data.Map
import Expr
import Func
import List
import Statem
import Text.Megaparsec
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration
import VarDeclares



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
    test04 -- Expr
    test05 -- Expr
    test06 -- Expr
    test07 -- Expr
    test08 -- Statem
    test09 -- Statem
    test10 -- Statem
    test11 -- Statem
    test12 -- Func
    test13 -- Func
    test14 -- Expr
    test15 -- Func
    test16 -- Func
    test17
    test18 -- Expr
    test19
    test20 -- VarDeclare
    test21 -- Statem
    test22 -- Expr
    test23 -- VarDeclares
    test24 -- Statem
    test25 -- Expr
    test26 -- Expr
    test27 -- Func
    test28 -- List
