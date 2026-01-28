module Text.Megaparsec.JS.Space where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Types

lscn1 :: JSParser ()
lscn1 = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lscn :: JSParser ()
lscn = L.space hspace (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

scn :: JSParser a -> JSParser a
scn = L.lexeme lscn

scn1 :: JSParser a -> JSParser a 
scn1 = L.lexeme lscn1
