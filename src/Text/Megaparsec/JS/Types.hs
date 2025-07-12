module Text.Megaparsec.JS.Types where

import Text.Megaparsec
import Data.Void
import Data.Text as T
import Control.Monad.State as S

type Parser = ParsecT Void Text (S.State ParserState) 

data ParserState = ParserState { scopeLevel :: Int, scopePos :: Int, currentFuncName :: Text } deriving(Show, Eq)

data Variable = GlobalVar Text | LocalVar { varFunctionName :: Text, varName :: Text, varScopeLevel :: Int, varScopePos :: Int} deriving(Show, Eq)
