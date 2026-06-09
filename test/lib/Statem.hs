module Statem where

import Control.Monad.State
import Data.Either
import Data.Map 
import Text.Megaparsec
import Text.Megaparsec.JS
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types

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

test24 :: IO ()
test24 = do
    let input = "var d = 1 ; \n; "
        result = runParserT (jsStatem <* eof)  "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    case result' of
        (Right result2) -> do
            if Data.Map.member "d" (variables newState) 
            then
                putStrLn ("Test 24 succeeded.")
            else
                putStrLn ("Test 24 failed.")
        (Left e) -> putStrLn (errorBundlePretty e)

test21 :: IO ()
test21 = do
    let input = "document.write(\"screw html >:c\");"
        result = runParserT jsStatem "" input
        (result', _) = runState result jsInitialState 
        correctAnswer = ExprStatem (FuncCallExpr (BinOpExpr (VarExpr (GlobalVar {gvVarName = "document", gvMethods = [("write",1)]})) (VarExpr (UnknownVar "write")) MemAccBinOp) [StringLitExpr "screw html >:c"])
    if isRight result'
    then do
        let (Right result'') = result'
        if result'' == correctAnswer
        then
            putStrLn "Test 21 succeeded."
        else
            putStrLn ("Test 21 failed. Got: " ++ (show result''))
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)
