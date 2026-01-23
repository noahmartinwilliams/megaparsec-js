module Text.Megaparsec.JS.Types where

import Text.Megaparsec
import Data.Void
import Data.Map
import Control.Monad.State as S

type JSParser = ParsecT Void String (S.State ParserState) 

data ParserState = ParserState { scopePath :: [Int], variables :: Map String [Variable], scopeLevel :: Int, scopePos :: Int, currentFuncName :: String } deriving(Show, Eq)

data Variable = UnknownVar String | GlobalVar String | LocalVar { varPath :: [Int], varFunctionName :: String, varName :: String, varScopeLevel :: Int, varScopePos :: Int} deriving(Show, Eq)

data BinOp = MemAccBinOp | AssignBinOp | AddBinOp | SubBinOp | MulBinOp | DivBinOp deriving(Show, Eq)

data Expr = AnonFuncExpr [Variable] Statem | FuncCallExpr Expr [Expr] | BinOpExpr Expr Expr BinOp | VarExpr Variable | IntExpr Int deriving(Show, Eq)

data Statem = IfStatem Expr Statem | EmptyStatem | BlockStatem Statem Statem | WhileStatem Expr Statem | ReturnStatem Expr | VarDeclareStatem [(Variable, Maybe Expr)] deriving(Show, Eq)

data Funct = Funct String [Variable] Statem  deriving(Show, Eq)

data Doc = Doc [Either Funct [(Variable, Maybe Expr)]] deriving(Show, Eq)
