module Text.Megaparsec.JS.Doc(jsDoc) where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types
import {-# SOURCE #-} Text.Megaparsec.JS.VarDeclaration
import Text.Megaparsec.JS.Func

jsDocVarDeclare :: JSParser DocEntry
jsDocVarDeclare = do
    (VarDeclareStatem v) <- scn1 jsVarDeclarationSimple
    return (DocVarExpr v)


jsDocFunc :: JSParser DocEntry
jsDocFunc = do
    f <- scn1 jsFunc
    return (DocFunct f)

jsDocStatems :: JSParser DocEntry
jsDocStatems = do
    s <- scn1 jsStatems
    return (DocStatems s)

jsDoc :: Bool -> JSParser (Doc, Text.Megaparsec.State String Void)
jsDoc False = do
    syms <- scn1 (some (try jsDocFunc <|> try jsDocStatems))
    st <- getParserState
    return ((Doc syms), st)
jsDoc True = do
    syms <- scn1 (some (try jsDocFunc <|> try jsDocStatems))
    void $ scn1 (string "</script>")
    st <- getParserState
    return ((Doc syms), st)

