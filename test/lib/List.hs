module List where

import Control.Monad.State
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Types

test28 :: IO ()
test28 = do
    let input = "[1, 'b', v]"
        result = runParserT jsExpr "" input
        (result', newState) = runState result (ParserState { scopePath = [1], variables = Data.Map.empty, scopeLevel = 1, scopePos = 0, currentFuncName = "foo" })
    case result' of
        (Right (ListLitExpr [IntExpr 1,StringLitExpr "b",VarExpr (UnknownVar "v")])) -> putStrLn ("test 28 succeeded.")
        (Left e) -> putStrLn (errorBundlePretty e)
        _ -> putStrLn ("test 28 failed.")
