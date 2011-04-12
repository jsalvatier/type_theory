
module Untyped where

import Text.ParserCombinators.Parsec

data Info = Info Int deriving (Show)

data Term = TmVar Int 
          | TmAbs Term
          | TmApp Term Term  
{--
data VarBinding = VarBinding String 
newtype Context = [VarBinding] 


printtm :: Context -> Term -> String
printtm ctx t = case t of 
                   TmAbs t1 -> 
                     "(lambda " ++ (printtm (ctx +1) t1) ++ ")"
                 | TmApp t1 t2 -> 
                     "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"
                 | TmVar n -> show(n)
                 
eval1 ctx tm = case tm of
               TmApp t1 t2 | isalue t2 -> eval1 
--}

word :: GenParser Char st String
word = many1 letter

source_fileP :: GenParser Char st [term]
source_fileP = 
        do terms <- many term
           eof
           return terms
           
termP :: Int -> GenParser Char st Term
termP depth = parenP depth <|>
              tmVarP depth <|>
              tmAbsP depth <|>
              tmAppP depth  
        

parenP :: GenParser Char st Term
parenP = do char '(' 
            term <- termP
            char ')'
            return term
            
tmAbsP :: Int -> GenParser Char st Term
tmAbsP depth = do string "lam " 
                  body <- termP (depth +1)
                  return (TmAbs body)   
                  
natural = do var <- many digit
             return (readInt var)
              
tmVarsP depth :: Int -> GenParser Char st Term
tmVarsP depth = natural
            
         
                 

