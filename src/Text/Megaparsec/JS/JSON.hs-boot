module Text.Megaparsec.JS.JSON where

import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.String
import Text.Megaparsec.JS.Types

jsJSON :: JSParser Expr
