module Text.Megaparsec.JS(
module Text.Megaparsec.JS.Doc,
module Text.Megaparsec.JS.Expr,
module Text.Megaparsec.JS.Statem,
module Text.Megaparsec.JS.String,
module Text.Megaparsec.JS.Types,
jsInitialState
) where

import Data.Map
import Data.Text as T
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.String
import Text.Megaparsec.JS.Types

jsInitialState :: ParserState
jsInitialState = ParserState { scopePath = [0], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = "" }
