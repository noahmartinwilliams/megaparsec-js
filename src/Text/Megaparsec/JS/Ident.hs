module Text.Megaparsec.JS.Ident(jsIdent) where

import Text.Megaparsec
import Data.Set as Set
import Data.Text as T
import Data.Void
import Text.Megaparsec.JS.Space as S
import Text.Megaparsec.JS.Types
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

keywords :: Set Text
keywords = do
    let l = ["var", "let", "function", "return"]
        l' = Prelude.map T.pack l
    Set.fromList l'

ident :: Parser Text
ident = do
    fchar <- S.lexeme letterChar
    rest <- many (alphaNumChar <|> (single '_'))
    return (T.pack ([fchar] ++ rest))

-- Special thanks to ChatGPT for helping with this.
-- https://chatgpt.com/share/68a3a560-5b34-8009-80fd-9059f7767205

jsIdent :: Parser Text
jsIdent = do
    i <- ident
    if (Set.member i keywords)
    then
        fail $ "keyword " <> show i <> " cannot be an identifier."
    else
        return i
