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
    void $ scn1 (string "function")
    funcName <- scn1 jsIdent
    put (pstate { currentFuncName = funcName } )
    void $ scn1 (single '(')
    args <- scn1 (try (jsArgList <|> jsArg1))
    void $ scn1 (single ')')
    let newMapList = Prelude.zip (Prelude.map (\(LocalVar { varName = v}) -> v) args) (Prelude.map (\x -> [x]) args)
        newMap = M.fromList newMapList
    put (pstate { scopePath = ((spos + 1) : spath), variables = (M.union vars newMap), scopeLevel = 1, scopePos = (spos + 1)})
    void $ scn1 (single '{')
    s <- scn1 jsStatems
    void $ scn1 (single '}')
    put (pstate { currentFuncName = "", scopePath = spath, variables = vars, scopeLevel = 0})
    return (Funct funcName args s)

jsArg1 = do
    a <- jsArg
    return [a]

comma :: JSParser ()
comma = do
    void $ scn1 (single ',')
    return ()

jsArgList = sepBy jsArg comma

jsArg = do
    (ParserState { scopePath = spath, scopeLevel = slevel, scopePos = spos, currentFuncName = cfn }) <- get
    ident <- jsIdent
    return (LocalVar { varPath = ((spos + 1) : spath), varFunctionName = cfn, varName = ident, varScopeLevel = (slevel + 1), varScopePos = (spos + 1) })
