module Grammar where 


data Expr = Var String | Add Expr Expr | Mult Expr Expr deriving Show

