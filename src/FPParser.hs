module FPParser where
import SyntaxTree 

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language 

languagedef = haskellDef {P.reservedNames   = ["lam","."],
                          P.reservedOpNames = []
                          }


-- the parser
     
parseTerm :: Parser Term
parseTerm =  (parens parseTerm)
         <|> parseTmVar
         <|> parseTmApp
         <|> parseTmAbs 

         
parseTmVar :: Parser Term
parseTmVar = do varname <- identifier
                return (TmVar varname)
         
parseTmAbs :: Parser Term      
parseTmAbs = do reserved "lam"
                varname <- identifier
                reserved "."
                body <- parseTerm
                return (TmAbs varname body)

parseTmApp :: Parser Term
parseTmApp = do tm1 <- parseTerm
                tm2 <- parseTerm
                return (TmApp tm1 tm2) 
                
program :: Parser [Term]
program = semiSep1 parseTerm

fileOf :: Parser a -> Parser a 
fileOf p = do whiteSpace
              x <- p
              eof
              return x

-- the lexer 
lexer       = P.makeTokenParser languagedef  

-- this is basically a substitute for parametarized modules
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
semiSep1    = P.semiSep1 lexer
whiteSpace  = P.whiteSpace lexer
                          
fileAST :: String -> IO (Either ParseError [Term])
fileAST path = do source <- readFile path
                  return (parse (fileOf program) path source)