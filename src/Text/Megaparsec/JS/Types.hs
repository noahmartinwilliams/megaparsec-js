module Text.Megaparsec.JS.Types where

import Text.Megaparsec
import Data.Void
import Data.Text as T
import Data.Map
import Control.Monad.State as S

type Parser = ParsecT Void Text (S.State ParserState) 

data ParserState = ParserState { scopePath :: [Int], variables :: Map Text [Variable], scopeLevel :: Int, scopePos :: Int, currentFuncName :: Text } deriving(Show, Eq)

data Variable = UnknownVar Text | GlobalVar Text | LocalVar { varPath :: [Int], varFunctionName :: Text, varName :: Text, varScopeLevel :: Int, varScopePos :: Int} deriving(Show, Eq)

data BinOp = MemAccBinOp | AssignBinOp | AddBinOp | SubBinOp | MulBinOp | DivBinOp deriving(Show, Eq)

data Expr = BinOpExpr Expr Expr BinOp | VarExpr Variable | IntExpr Int deriving(Show, Eq)

data Statement = VarDeclare [(Variable, Maybe Expr)] deriving(Show, Eq)
