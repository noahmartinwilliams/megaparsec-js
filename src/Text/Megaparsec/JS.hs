module Text.Megaparsec.JS(
module Text.Megaparsec.JS.Doc,
module Text.Megaparsec.JS.Types,
jsInitialState
) where

import Data.Map
import Data.Text as T
import Text.Megaparsec.JS.Doc
import Text.Megaparsec.JS.Types

jsInitialState :: ParserState
jsInitialState = ParserState { scopePath = [0], variables = Data.Map.empty, scopeLevel = 0, scopePos = 0, currentFuncName = (T.pack "") }
