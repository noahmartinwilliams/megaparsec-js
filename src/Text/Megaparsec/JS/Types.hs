module Text.Megaparsec.JS.Types where

import Text.Megaparsec
import Data.Void
import Data.Map
import Control.Monad.State as S

type JSParser = ParsecT Void String (S.State ParserState) 

data ParserState = ParserState { scopePath :: [Int], variables :: Map String [Variable], functions :: Map String Funct, scopeLevel :: Int, scopePos :: Int, currentFuncName :: String } deriving(Show, Eq)

data Variable = UnknownVar String | 
    GlobalVar {gvVarName :: String, gvMethods :: [(String, Int)]} | 
    LocalVar { varPath :: [Int], varFunctionName :: String, varName :: String, varScopeLevel :: Int, varScopePos :: Int} deriving(Show, Eq)

data BinOp = EqualityBinOp | 
    MemAccBinOp | 
    AssignBinOp | 
    AddBinOp | 
    SubBinOp | 
    MulBinOp | 
    DivBinOp | 
    LogAndBinOp |
    LogOrBinOp | 
    StrictInequalityBinOp deriving(Show, Eq)

data Expr = BoolExpr Bool | 
    TernaryExpr Expr Expr Expr | 
    AnonFuncExpr [Variable] Statem | 
    FuncCallExpr Expr [Expr] | 
    NewExpr Expr |
    TypeOfExpr Expr |
    BinOpExpr Expr Expr BinOp | 
    VarExpr Variable | 
    IntExpr Int | 
    StringLitExpr String | 
    FuncExpr Funct | 
    ListLitExpr [Expr] |
    ObjExpr (Map String Expr) deriving(Show, Eq)

data Statem = IfStatem Expr Statem | EmptyStatem | BlockStatem Statem Statem | WhileStatem Expr Statem | ReturnStatem (Maybe Expr) | VarDeclareStatem [(Variable, Maybe Expr)] | ExprStatem Expr deriving(Show, Eq)

data Funct = DateFunct | Funct String [Variable] Statem  deriving(Show, Eq)

data DocEntry = DocFunct Funct | DocVarExpr [(Variable, Maybe Expr)] | DocStatems Statem deriving(Show, Eq)

data Doc = Doc [DocEntry] deriving(Show, Eq)
