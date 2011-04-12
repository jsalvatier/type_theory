
module FPParser where
import SyntaxTree 

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language


-- these two statements basically do the same thing as a parametarized module apparently
def = haskellDef {reservedNames   = ["lam","."],
                  reservedOpNames = []
                 }

TokenParser{parens = m_parens,
            identifier = m_identifier,
            reservedOp = m_reservedOp,
            reserved = m_reserved,
            semiSep1 = m_semiSep1,
            whiteSpace = m_whiteSpace }  = makeTokenParser def

     
parseTerm :: Parser Term
parseTerm =  (m_parens parseTerm)
         <|> parseTmVar
         <|> parseTmAbs 
         <|> parseTmApp
         
parseTmVar :: Parser Term
parseTmVar = do varname <- m_identifier
                return (TmVar varname)
         
parseTmAbs :: Parser Term      
parseTmAbs = do m_reserved "lam"
                varname <- m_identifier
                m_reserved "."
                body <- parseTerm
                return (TmAbs varname body)

parseTmApp :: Parser Term
parseTmApp = do tm1 <- parseTerm
                tm2 <- parseTerm
                return (TmApp tm1 tm2) 
                
parseFile :: Parser [Term]
parseFile = m_whiteSpace >>
            (m_semiSep1 parseTerm)
   