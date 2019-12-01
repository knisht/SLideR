{
module TempLexer where
}

%wrapper "basic"

$digit      = 0-9
$alpha      = [a-zA-Z]

tokens :-
    $white+                             ;
    $alpha [$alpha $digit '_']*         { \s -> VarT s }
    \+                                  { \_ -> PlusT }
    \(                                  { \_ -> LBraceT }
    \)                                  { \_ -> RBraceT }

{

data Token = VarT String 
           | PlusT
           | LBraceT
           | RBraceT
           deriving Show
}
