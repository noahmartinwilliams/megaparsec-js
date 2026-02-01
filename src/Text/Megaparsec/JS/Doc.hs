module Text.Megaparsec.JS.Doc(jsDoc) where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.VarDeclaration
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

lineComment :: JSParser ()
lineComment = do
    void $ string "//"
    void $ manyTill L.charLiteral (single '\n')
    return ()

jsDoc :: JSParser (Doc, Text.Megaparsec.State String Void)
jsDoc = do
    void $ optional (lineComment)
    syms <- scn1 (many (jsDocFunc <|> jsDocVarDeclare <|> jsDocStatems))
    void $ optional (lineComment)
    st <- getParserState
    return ((Doc syms), st)

