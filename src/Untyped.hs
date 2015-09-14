
module Untyped where

import Data.List
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Common

------------------------
-- Ejercicio 1
------------------------

num :: Integer -> LamTerm
num n =  undefined

-----------------------
--- Sección 2 Parsers
-----------------------

totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace untyped
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
untyped :: TokenParser u
untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_',
                                          reservedNames = ["def"] })

 
-- Parser para comandos
parseStmt :: Parser a -> Parser (Stmt a)
parseStmt p = do
          reserved untyped "def"
          x <- identifier untyped
          reservedOp untyped "="
          t <- p
          return (Def x t)
    <|> fmap Eval p

 
parseTermStmt :: Parser (Stmt Term)
parseTermStmt = fmap (fmap conversion) (parseStmt parseLamTerm)

-- Parser para LamTerms 
parseLamTerm :: Parser LamTerm
parseLamTerm = undefined 

-- conversion a términos localmente sin nombres
conversion  :: LamTerm -> Term
conversion = undefined

-- para testear el parser interactivamente.
testParser :: Parser LamTerm
testParser = totParser parseLamTerm                                    

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp = undefined

 
eval :: [(Name,Value)] -> Term -> Value
eval  e t = eval' t (e,[])

eval' :: Term -> (NameEnv Value,[Value]) -> Value
eval' (Bound  ii)  d  =  (snd d) !! ii
eval' t            d  = undefined

-------------------------------
-- Sección 4
-------------------------------

quote  :: Value -> Term
quote  =  undefined
