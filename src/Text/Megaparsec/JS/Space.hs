module Text.Megaparsec.JS.Space where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Types

scn1 :: JSParser ()
scn1 = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

scn :: JSParser ()
scn = L.space hspace (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
