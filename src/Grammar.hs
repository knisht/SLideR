module Grammar where 


data Expr = Var String | Add Expr Expr deriving Show
