module Text.Megaparsec.JS.JSON where

import Control.Monad
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS.Expr
import Text.Megaparsec.JS.Ident
import Text.Megaparsec.JS.Space as S
import Text.Megaparsec.JS.String
import Text.Megaparsec.JS.Types

jsJSON1 :: JSParser Expr
jsJSON1 = do
    void $ scn1 (single '{')
    entries <- jsJSONEntries
    void $ scn1 (single '}')
    return (ObjExpr (Data.Map.fromList entries))

jsJSON2 :: JSParser Expr
jsJSON2 = do
    void $ scn1 (single '{')
    void $ scn1 (single '}')
    return (ObjExpr (Data.Map.empty))

jsJSON = (try jsJSON2 <|> try jsJSON1)

jsJSONEntries :: JSParser [(String, Expr)]
jsJSONEntries = do
    entries <- sepBy jsJSONEntry (scn1 (single ','))
    return (entries)

jsJSONEntry :: JSParser (String, Expr)
jsJSONEntry = do
    name <- scn1 jsIdent
    void $ scn1 (single ':')
    e <- scn1 jsExpr 
    return (name, e)
