module Text.Megaparsec.JS.Ident(jsIdent) where

import Control.Monad
import Data.Set as Set
import Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.JS.Types

keywords :: Set String
keywords = do
    let l = ["var", "let", "function", "return"]
    Set.fromList l

ident :: JSParser String
ident = do
    fchar <- letterChar
    rest <- many (alphaNumChar <|> (single '_'))
    return ([fchar] ++ rest)

-- Special thanks to ChatGPT for helping with this.
-- https://chatgpt.com/share/68a3a560-5b34-8009-80fd-9059f7767205

jsIdent :: JSParser String
jsIdent = do
    i <- ident
    if (Set.member i keywords)
    then
        fail $ "keyword " <> show i <> " cannot be an identifier."
    else
        return i
