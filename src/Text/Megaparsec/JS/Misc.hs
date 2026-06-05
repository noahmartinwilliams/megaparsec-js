module Text.Megaparsec.JS.Misc where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Types

parens :: JSParser a -> JSParser a
parens x = do
    scn1 $ between (string "(") (string ")") x

comma :: JSParser ()
comma = do
    void $ scn1 (single ',')
    return ()

squareBrackets :: JSParser a -> JSParser a
squareBrackets x = do
    scn1 $ between (string "[") (string "]") x
