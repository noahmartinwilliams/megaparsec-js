module Expr where

import Control.Monad.State
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration

test04 :: IO ()
test04 = do
    let input = "let v;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
        input2 = "v + 1"
        result2 = runParserT jsExpr "" input2
        (result2', _) = runState result2 newState
    case result2' of
        (Right (BinOpExpr _ _ bop)) -> do
            if bop == AddBinOp
            then
                putStrLn ("Test 04 succeeded.")
            else
                putStrLn ("Test 04 failed.")
        (Left err) -> putStrLn (errorBundlePretty err)

test05 :: IO ()
test05 = do
    let input = "let v;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
        input2 = "v . mem"
        result2 = runParserT jsExpr "" input2
        (result2', _) = runState result2 newState
    case result2' of 
        (Right result2'') -> do
            let (BinOpExpr _ _ op) = result2''
            if op == MemAccBinOp
            then
                putStrLn ("Test 05 succeeded.")
            else
                putStrLn ("Test 05 failed.")
        (Left err) -> putStrLn (errorBundlePretty err)

test06 :: IO ()
test06 = do
    let input = "let v;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
        input2 = "v = v"
        result2 = runParserT jsExpr "" input2
        (result2', _) = runState result2 newState
    case result2' of
        (Right result2'') -> do
            let (BinOpExpr _ _ op) = result2''
            if op == AssignBinOp
            then
                putStrLn ("Test 06 succeeded.")
            else
                putStrLn ("Test 06 failed.")
        (Left err) -> putStrLn (errorBundlePretty err)

test07 :: IO ()
test07 = do
    let input = "let v;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
        input2 = "v = v * v + v"
        result2 = runParserT jsExpr "" input2
        (result2', _) = runState result2 newState
    case result2' of
        (Right result2'') -> do
            let (BinOpExpr _ (BinOpExpr _ _ op2) _) = result2''
            if op2 == AddBinOp
            then
                putStrLn ("Test 07 succeeded.")
            else
                putStrLn ("Test 07 failed. Got: \"" ++ (show op2) ++ "\".")
        (Left err) -> putStrLn (errorBundlePretty err)


test14 :: IO ()
test14 = do
    let input = "func(1, 2)"
        result = runParserT jsExpr "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of 
        (Right (FuncCallExpr _ args)) -> do
            if (args !! 0) == (IntExpr 1)
            then
                putStrLn ("Test 14 succeeded.")
            else
                putStrLn ("Test 14 failed.")
        (Left err) -> putStrLn (errorBundlePretty err)

test18 :: IO ()
test18 = do
    let input = "1 == 2"
        result = runParserT jsExpr "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right (BinOpExpr (IntExpr 1) (IntExpr 2) EqualityBinOp)) ->
                putStrLn ("Test 18 succeeded.")
        (Left err) -> putStrLn (errorBundlePretty err)
        _ -> putStrLn ("Test 18 failed.")

test22 :: IO ()
test22 = do
    let input = "new Date()"
        result = runParserT jsExpr "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right (NewExpr (FuncCallExpr (VarExpr (UnknownVar "Date")) []))) -> putStrLn ("Test 22 succeeded.")
        (Left err) -> putStrLn (errorBundlePretty err)
        _ -> putStrLn ("Test 22 failed. Got: \"" ++ (show result') ++ "\".")

test25 :: IO ()
test25 = do
    let input = "(x == 1) && (y == 2)"
        result = runParserT jsExpr "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right result'') -> do
            if result'' == (BinOpExpr (BinOpExpr (VarExpr (UnknownVar "x")) (IntExpr 1) EqualityBinOp) (BinOpExpr (VarExpr (UnknownVar "y")) (IntExpr 2) EqualityBinOp) LogAndBinOp)
            then
                putStrLn ("Test 25 succeeded.")
            else
                putStrLn ("Test 25 failed. Got: \"" ++ (show result') ++ "\".")
        (Left err) -> putStrLn (errorBundlePretty err)

test26 :: IO ()
test26 = do
    let input = "(x == 1) !== 1"
        result = runParserT jsExpr "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" })
    case result' of
        (Right result'') -> do
            if result'' == (BinOpExpr (BinOpExpr (VarExpr (UnknownVar "x")) (IntExpr 1) EqualityBinOp) (IntExpr 1) StrictInequalityBinOp)
            then
                putStrLn ("Test 26 succeeded.")
            else
                putStrLn ("Test 26 failed. Got: \"" ++ (show result') ++ "\".")
        (Left err) -> putStrLn (errorBundlePretty err)
