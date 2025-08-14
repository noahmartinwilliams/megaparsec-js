module Text.Megaparsec.JS.Func where

import Control.Monad
import Control.Monad.State
import Data.Map as M
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.JS.Types
import Text.Megaparsec.JS.Statem
import Text.Megaparsec.JS.Space as S

jsFunc :: Parser Function
jsFunc = do
    pstate@(ParserState { scopePath = spath, variables = vars, scopeLevel = slevel, scopePos = spos}) <- get
    void $ S.lexeme (string (T.pack "function"))
    f <- S.lexeme (letterChar)
    restName <- many (alphaNumChar)
    let funcName = T.pack ([f] ++ restName)
    put (pstate { currentFuncName = funcName } )
    void $ S.lexeme (single '(')
    args <- (try (jsArgList <|> jsArg1))
    void $ S.lexeme (single ')')
    let newMapList = Prelude.zip (Prelude.map (\(LocalVar { varName = v}) -> v) args) (Prelude.map (\x -> [x]) args)
        newMap = M.fromList newMapList
    put (pstate { scopePath = (spos : spath), variables = (M.union vars newMap), scopeLevel = 1, scopePos = (spos + 1)})
    void $ S.lexeme (single '{')
    s <- jsStatems
    void $ S.lexeme (single '}')
    put (pstate { currentFuncName = (T.pack ""), scopePath = spath, variables = vars, scopeLevel = 0})
    return (Function funcName args s)


jsArg1 :: Parser [Variable]
jsArg1 = some jsArg

jsArgList :: Parser [Variable]
jsArgList = sepBy jsArg (S.lexeme (single ','))

jsArg :: Parser Variable
jsArg = do
    (ParserState { scopePath = spath, scopeLevel = slevel, scopePos = spos, currentFuncName = cfn }) <- get
    f <- S.lexeme letterChar
    frest <- many alphaNumChar
    return (LocalVar { varPath = ((spos + 1) : spath), varFunctionName = cfn, varName = (T.pack ([f] ++ frest)), varScopeLevel = (slevel + 1), varScopePos = (spos + 1) })
