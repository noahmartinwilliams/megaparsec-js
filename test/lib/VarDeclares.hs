module VarDeclares where

import Control.Monad.State
import Data.Either
import Data.Map as Map
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.JS
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Func
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration

test01 :: IO ()
test01 = do
    let input = "let v;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', _) = runState result (ParserState { scopePath = [1], variables = Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
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
        allVars = Map.fromList [("v1", [v1Var]), ("v2", [v2Var])]
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
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
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


test19 :: IO ()
test19 = do
    let input = "let v = 1, a;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', _) = runState result (ParserState { scopePath = [1], variables = Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    if isRight result'
    then do
        let (Right result'') = result'
        let (VarDeclareStatem ((LocalVar { varName = vname }, _) : _)) = result''
        if (vname == "v" ) || (vname == "a")
        then
            putStrLn ("Test 19 succeeded.")
        else
            putStrLn ("Test 19 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)

test20 :: IO ()
test20 = do
    let input = "let v = document, a;"
        result = runParserT jsVarDeclarationSimple "" input
        (result', st) = runState result jsInitialState 
        doc = Map.lookup "document" (variables st)
    if isRight result'
    then 
        if isJust doc 
        then
            putStrLn "Test 20 succeeded."
        else
            putStrLn "Test 20 failed."
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)


test23 :: IO ()
test23 = do
    let input = "var _sf_startpt=(new Date()).getTime();"
        result = runParserT jsStatem "" input
        (result', _) = runState result jsInitialState 
    case result' of
        (Right (VarDeclareStatem [(LocalVar {varPath = [0], varFunctionName = "", varName = "_sf_startpt", varScopeLevel = 1, varScopePos = 0},Just _ )])) ->
            putStrLn ("Test 23 succeeded.")
        (Left err) -> putStrLn (errorBundlePretty err)
        (Right r) -> putStrLn ("Test 23 failed. Got: " ++ (show r))
