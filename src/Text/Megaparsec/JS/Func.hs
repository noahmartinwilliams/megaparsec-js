module Text.Megaparsec.JS.Func where

import Control.Monad
import Control.Monad.State
import Data.Map as M
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Space
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.Ident

jsFunc :: JSParser Funct
jsFunc = do
    pstate@(ParserState { scopePath = spath, variables = vars, scopeLevel = slevel, scopePos = spos}) <- get
    void $ scn
    void $ (string "function")
    void $ scn1
    f <- (letterChar)
    restName <- many (alphaNumChar)
    let funcName = ([f] ++ restName)
    put (pstate { currentFuncName = funcName } )
    void $ (single '(')
    void $ scn
    args <- (try (jsArgList <|> jsArg1))
    void $ scn
    void $ (single ')')
    let newMapList = Prelude.zip (Prelude.map (\(LocalVar { varName = v}) -> v) args) (Prelude.map (\x -> [x]) args)
        newMap = M.fromList newMapList
    put (pstate { scopePath = (spos : spath), variables = (M.union vars newMap), scopeLevel = 1, scopePos = (spos + 1)})
    void $ (single '{')
    void $ scn
    s <- jsStatems
    void $ scn
    void $ (single '}')
    put (pstate { currentFuncName = "", scopePath = spath, variables = vars, scopeLevel = 0})
    return (Funct funcName args s)

jsArg1 = some jsArg

comma :: JSParser ()
comma = do
    void $ scn
    void $ single ','
    void $ scn
    return ()

jsArgList = sepBy jsArg comma

jsArg = do
    (ParserState { scopePath = spath, scopeLevel = slevel, scopePos = spos, currentFuncName = cfn }) <- get
    ident <- jsIdent
    return (LocalVar { varPath = ((spos + 1) : spath), varFunctionName = cfn, varName = ident, varScopeLevel = (slevel + 1), varScopePos = (spos + 1) })
