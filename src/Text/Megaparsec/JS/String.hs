module Text.Megaparsec.JS.String where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types

jsStringLit :: JSParser Expr
jsStringLit = do
    void $ single '"'
    content <- manyTill (char '\\' *> char '"' <|> noneOf "\"") (char '"')
    return (StringLitExpr content)
