module Text.Megaparsec.JS.List where

import Text.Megaparsec
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Misc
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types


jsListLit = do
    exprs <- squareBrackets ((scn1 jsExpr) `sepBy` (scn1 comma))
    return (ListLitExpr exprs)
