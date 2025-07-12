module Text.Megaparsec.JS.Space where

import Text.Megaparsec
import Data.Text as T
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Types

sc :: Parser ()
sc = L.space space1 Text.Megaparsec.empty Text.Megaparsec.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
