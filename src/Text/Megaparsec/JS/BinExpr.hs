module Text.Megaparsec.JS.BinExpr where

import Text.Megaparsec.JS.Types

mkMulExpr :: Expr -> Expr -> Expr
mkMulExpr e1 e2 = BinOpExpr e1 e2 MulBinOp

mkDivExpr :: Expr -> Expr -> Expr
mkDivExpr e1 e2 = BinOpExpr e1 e2 DivBinOp

mkAddExpr :: Expr -> Expr -> Expr
mkAddExpr e1 e2 = BinOpExpr e1 e2 AddBinOp

mkSubExpr :: Expr -> Expr -> Expr
mkSubExpr e1 e2 = BinOpExpr e1 e2 SubBinOp

mkAssignExpr :: Expr -> Expr -> Expr
mkAssignExpr e1 e2 = BinOpExpr e1 e2 AssignBinOp

mkMemAccExpr :: Expr -> Expr -> Expr
mkMemAccExpr e1 e2 = BinOpExpr e1 e2 MemAccBinOp

mkEqualityExpr :: Expr -> Expr -> Expr
mkEqualityExpr e1 e2 = BinOpExpr e1 e2 EqualityBinOp

mkLogAndExpr :: Expr -> Expr -> Expr
mkLogAndExpr e1 e2 = BinOpExpr e1 e2 LogAndBinOp

mkLogOrExpr :: Expr -> Expr -> Expr
mkLogOrExpr e1 e2 = BinOpExpr e1 e2 LogOrBinOp

mkStrictInequalityExpr :: Expr -> Expr -> Expr
mkStrictInequalityExpr e1 e2 = BinOpExpr e1 e2 StrictInequalityBinOp

mkLTExpr :: Expr -> Expr -> Expr
mkLTExpr e1 e2 = BinOpExpr e1 e2 LTBinOp

mkGTExpr :: Expr -> Expr -> Expr
mkGTExpr e1 e2 = BinOpExpr e1 e2 GTBinOp

mkStrictEqualityExpr :: Expr -> Expr -> Expr
mkStrictEqualityExpr e1 e2 = BinOpExpr e1 e2 StrictEqualityBinOp
