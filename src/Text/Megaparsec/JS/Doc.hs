module Text.Megaparsec.JS.Doc(jsDoc) where

import Data.Text
import Data.Void
import Text.Megaparsec.JS.Types
import Text.Megaparsec
import Text.Megaparsec.JS.VarDeclaration
import Text.Megaparsec.JS.Func

jsDocVarDeclare :: Parser (Either Funct [(Variable, Maybe Expr)])
jsDocVarDeclare = do
    (VarDeclareStatem v) <- varDeclarationSimple
    return (Right v)


jsDocFunc :: Parser (Either Funct [(Variable, Maybe Expr)])
jsDocFunc = do
    f <- jsFunc
    return (Left f)

jsDoc :: Parser (Doc, Text.Megaparsec.State Text Void)
jsDoc = do
    syms <- (some (jsDocFunc <|> jsDocVarDeclare))
    st <- getParserState
    return ((Doc syms), st)
