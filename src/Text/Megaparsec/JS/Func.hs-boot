module Text.Megaparsec.JS.Func where

import Text.Megaparsec
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.Space
import Text.Megaparsec.Char
import Data.Text as T
import Data.Map as M
import Control.Monad
import Control.Monad.State

jsArg1 :: Parser [Variable]

jsArgList :: Parser [Variable]

jsArg :: Parser Variable
