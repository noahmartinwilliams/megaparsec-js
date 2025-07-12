module Main where

import Text.Megaparsec
import Data.Text as T
import Data.Either
import Text.Megaparsec.JS.VarDeclaration
import Text.Megaparsec.JS.Types
import Control.Monad.State

test01 :: IO ()
test01 = do
    let input = "let v;"
        result = runParserT varDeclaration "" (T.pack input)
        (result', _) = runState result (ParserState { scopeLevel = 1, scopePos = 0, currentFuncName = (T.pack "foo") })
    if isRight result'
    then do
        let (Right result'') = result'
        let [(LocalVar { varName = vname })] = result''
        if vname == (T.pack "v")
        then
            putStrLn ("Test 01 succeeded.")
        else
            putStrLn ("Test 01 failed.")
    else
        let (Left err) = result' in putStrLn (errorBundlePretty err)


main :: IO ()
main = do
    test01
