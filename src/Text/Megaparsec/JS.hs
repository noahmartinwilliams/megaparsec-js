module Text.Megaparsec.JS(
module Text.Megaparsec.JS.Doc,
module Text.Megaparsec.JS.Expr,
module Text.Megaparsec.JS.Statem,
module Text.Megaparsec.JS.String,
module Text.Megaparsec.JS.Types,
jsInitialState
) where

import Data.Map as Map
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.String
import Text.Megaparsec.JS.Types

jsInitialState :: ParserState
jsInitialState = do
    let docVar = ("document", [GlobalVar { gvVarName = "document", gvMethods = [("write", 1)]}])
        initialVars = Map.fromList [docVar]
    ParserState { scopePath = [0], variables = initialVars, scopeLevel = 0, scopePos = 0, currentFuncName = "" }
