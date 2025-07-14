module Text.Megaparsec.JS.Expr where

import Text.Megaparsec
import Text.Megaparsec.JS.Space as S
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration
import Data.Text as T
import Control.Monad.State

jsExprVar :: Parser Expr
jsExprVar = do
    stateVar <- get
    firstChar <- S.lexeme letterChar
    restOfName <- many alphaNumChar
    let name = [firstChar] ++ restOfName
        varLookedUp = lookupVar (T.pack name) stateVar
    return (VarExpr varLookedUp)
