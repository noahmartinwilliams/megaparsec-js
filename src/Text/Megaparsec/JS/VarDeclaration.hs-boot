module Text.Megaparsec.JS.VarDeclaration where

import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS.Types


lookupVar :: String -> ParserState -> Variable
jsVarDeclarationSimple :: JSParser Statem
insertVar :: Map String [Variable] -> Variable -> Map String [Variable]
