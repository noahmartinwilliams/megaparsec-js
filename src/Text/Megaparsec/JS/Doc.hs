module Text.Megaparsec.JS.Doc where

import Text.Megaparsec.JS.Types
import Text.Megaparsec
import Text.Megaparsec.JS.VarDeclaration
import Text.Megaparsec.JS.Func

jsDocVarDeclare :: Parser (Either Function [(Variable, Maybe Expr)])
jsDocVarDeclare = do
    (VarDeclareStatem v) <- varDeclarationSimple
    return (Right v)


jsDocFunc :: Parser (Either Function [(Variable, Maybe Expr)])
jsDocFunc = do
    f <- jsFunc
    return (Left f)

jsDoc :: Parser Doc
jsDoc = do
    syms <- (some (jsDocFunc <|> jsDocVarDeclare))
    return (Doc syms)
