module Func where

import Control.Monad.State
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Types

test12 :: IO ()
test12 = do
    let input = "function myFunc(a, b) { return a + b ; }"
        result = runParserT jsFunc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "", functions = Data.Map.empty })
    case result' of
        (Right (Funct fname _ _ ) ) -> do
            if fname == "myFunc"
            then
                putStrLn ("Test 12 succeeded.")
            else
                putStrLn ("Test 12 failed.")
        (Left err) -> putStrLn (errorBundlePretty err)

test13 :: IO ()
test13 = do
    let input = "function myFunc(a, b) { return a + b ; }"
        result = runParserT jsFunc "" input
        (result', _) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right (Funct _ vars _)) -> do
            if (vars !! 0) == LocalVar { varPath = [1, 1], varFunctionName = "myFunc", varName = "a", varScopeLevel = 1, varScopePos = 1}
            then
                putStrLn ("Test 13 succeeded.")
            else
                putStrLn ("Test 13 failed. Got : " ++ (show vars))
        (Left err) -> putStrLn (errorBundlePretty err)


test15 :: IO ()
test15 = do
    let input = "function f(a) { return a + 1; } var v ;"
        result = runParserT jsDoc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right ((Doc syms), _)) -> do
            if (Prelude.length syms) == 2
            then
                putStrLn ("Test 15 succeeded.")
            else
                putStrLn ("Test 15 failed.")
        (Left err) -> putStrLn (errorBundlePretty err)

test16 :: IO ()
test16 = do
    let input = "function f(a) { return function(b) { return a + b; } ; }  var v ;"
        result = runParserT jsDoc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right ((Doc syms), _)) -> do
            if (Prelude.length syms) == 2
            then
                putStrLn ("Test 16 succeeded.")
            else
                putStrLn ("Test 16 failed.")
        (Left err) -> putStrLn (errorBundlePretty err)

test27 :: IO ()
test27 = do
    let input = "(function () { return 1; }());"
        result = runParserT jsDoc "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right ((Doc syms), _)) -> do
            if (syms !! 0) == DocStatems (BlockStatem (ExprStatem (FuncCallExpr (AnonFuncExpr [] (BlockStatem (ReturnStatem (Just (IntExpr 1))) EmptyStatem)) [])) EmptyStatem)
            then
                putStrLn ("Test 27 succeeded.")
            else
                putStrLn ("Test 27 failed. Got: " ++ (show result'))
        (Left err) -> putStrLn (errorBundlePretty err)


