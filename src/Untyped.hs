
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
num n = Abs "f" (Abs "x" (app n))
            where app 0 = (LVar "x")
                  app n = (App (LVar "f") (app (n-1)))

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
parseStmt p = do reserved untyped "def"
                 x <- identifier untyped
                 reservedOp untyped "="
                 t <- p
                 return (Def x t)
             <|> fmap Eval p

 
parseTermStmt :: Parser (Stmt Term)
parseTermStmt = fmap (fmap conversion) (parseStmt parseLamTerm)

{-  

--  Gramatica del calculo lambda extendido
    
    <atom>   := <var> | <number> | '(' <term> ')'

    <ids>    := <var> ( e | <ids> )

    <abs>    := '\' <ids> '.' <term>

    <notAbs> := <atom> | <notAbs> <notApp>
    
    <notApp> := <atom> | <abs>
    
    <term>   := <abs> | <notAbs>

 -- Gramatica del calculo lambda extendido sin recursion a izq
    
    <atom>   := <var> | <number> | '(' <term> ')'

    <ids>    := <var> ( e | <ids> )

    <abs>    := '\' <ids> '.' <term>

    <notAbs> := <atom> <notAbs'>
    <notAbs'> := <notApp> <notAbs'> | e
    
    <notApp> := <atom> | <abs>
    
    <term>   := <abs> | <notAbs>
    
-}


-- Parsers auxiliares para LamTerms
parseAtom :: Parser LamTerm
parseAtom = parens untyped parseLamTerm
        <|> do var <- identifier untyped
               return $ LVar var
            <|> do n <- read `fmap` many1 digit
                   return $ num n

parseIds :: Parser [String]
parseIds = many1 $ identifier untyped

parseAbs :: Parser LamTerm
parseAbs = do reservedOp untyped "\\"
              (i:ids) <- parseIds
              reservedOp untyped "."
              term <- parseLamTerm
              return $ Abs i (nest ids term)
                  where nest [] t = t
                        nest (x:xs) t = Abs x (nest xs t)

parseNotAbs :: Parser LamTerm
parseNotAbs = do atom <- parseAtom
                 f_nabs' <- parseNotAbs'
                 return $ f_nabs' atom
                 
parseNotAbs' :: Parser (LamTerm -> LamTerm)
parseNotAbs' = do napp <- parseNotApp
                  f_nabs' <- parseNotAbs'
                  return $ \x -> f_nabs' (App x napp)
           <|> return id
                  
parseNotApp :: Parser LamTerm
parseNotApp = parseAtom <|> parseAbs


-- Parser para LamTerms 
parseLamTerm :: Parser LamTerm
parseLamTerm = parseAbs <|> parseNotAbs
                
                
-- conversion a términos localmente sin nombres
conversion  :: LamTerm -> Term
conversion  = toTerm []

toTerm :: [String] -> LamTerm ->  Term
toTerm names (Abs name t) = Lam $ toTerm (name:names) t 
toTerm names (App t1 t2) = (toTerm names t1) :@: (toTerm names t2)
toTerm names (LVar name) = case elemIndex name names
                           of Just index -> Bound index
                              Nothing    -> Free $ Global name


-- para testear el parser interactivamente.
testParser :: Parser LamTerm
testParser = totParser parseLamTerm

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VNeutral t1) t2 = VNeutral $ NApp t1 t2
vapp (VLam f)      t2 = f t2

 
eval :: [(Name, Value)] -> Term -> Value
eval  e t = eval' t (e,[])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (t1 :@: t2)        d = vapp (eval' t1 d) (eval' t2 d)
eval' (Bound ii) (env, bv) = bv !! ii
eval' (Lam e)    (env, bv) = VLam (\x -> eval' e (env, (x:bv)))
eval' (Free n)   (env, bv) = lookFor n env
                                where lookFor n [] = VNeutral $ NFree $ n
                                      lookFor n ((k, v) : xs) = 
                                            if k == n
                                            then v
                                            else lookFor n xs


-------------------------------
-- Sección 4
-------------------------------

quote :: Value -> Term
quote v = quote' v 0

quote' :: Value -> Int -> Term
quote' (VNeutral (NFree (Global n))) i = Free $ Global n
quote' (VNeutral (NFree (Quote  n))) i = Bound (i - n - 1)
quote' (VNeutral (NApp  n v))        i = (quote' (VNeutral n) i) :@: (quote' v i)
quote' (VLam f)                      i = Lam $ quote' (f (VNeutral $ NFree $ Quote i)) (i+1)

