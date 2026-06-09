module Text.Megaparsec.JS.String where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types

jsStringLit1 :: JSParser Expr
jsStringLit1 = do
    void $ (single '"')
    content <- scn1 (manyTill (char '\\' *> char '"' <|> noneOf "\"") (char '"'))
    return (StringLitExpr content)

jsStringLit2 :: JSParser Expr
jsStringLit2 = do
    str <- scn1 (char '\'' *> manyTill charInString (char '\''))
    return (StringLitExpr str) where
        charInString = (char '\\' *> escapedChar) <|> satisfy (/= '\'')
        escapedChar =
            (char 'n' >> return '\n') <|>
            (char 't' >> return '\t') <|>
            (char '\'' >> return '\'' ) <|>
            (char '\\' >> return '\\')


jsStringLit :: JSParser Expr
jsStringLit = (try jsStringLit1 <|> try jsStringLit2)
