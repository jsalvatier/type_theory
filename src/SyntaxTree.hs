
module SyntaxTree where

data Term = TmVar String 
          | TmAbs String Term
          | TmApp Term Term

instance Show Term where
    show (TmVar varname)           = show varname
    show (TmAbs varname body)      = "(\\" ++ show varname ++ "." ++ (show body) ++ ")"
    show (TmApp fn val)         = "(" ++ (show fn) ++ " " ++ (show val) ++ ")"

                 
                   
data BruijnTerm = BtVar Int 
                | BtAbs BruijnTerm
                | BtApp BruijnTerm BruijnTerm                
               
instance Show BruijnTerm where
    show (BtVar varname)        = show varname
    show (BtAbs body)           = "(\\" ++ (show body) ++ ")"
    show (BtApp fn val)         = "(" ++ (show fn) ++ " " ++ (show val) ++ ")"

