{
module TemplateLexer where
}

%wrapper "basic"

$digit      = 0-9
$alpha      = [a-zA-Z]

tokens :-
    $white+                             ;
    "%module"                          { \_ -> ModuleDirectiveT }
    "%import"                          { \_ -> ImportDirectiveT }
    "%tokens"                          { \_ -> TokensDirectiveT }
    "%grammar"                         { \_ -> GrammarDirectiveT }
    "_"                                 { \_ -> UnderscoreT }
    \{                                  { \_ -> CurlyLBraceT }
    \}                                  { \_ -> CurlyRBraceT }
    \[                                  { \_ -> SquareLBraceT }
    \]                                  { \_ -> SquareRBraceT }
    \-\>                                { \_ -> ArrowT }
    \|                                  { \_ -> DelimiterT }
    \,                                  { \_ -> CommaT}
    \$ [$digit]+                        { \s -> RefT $ read (tail s) }                 
    $alpha [$alpha $digit]*             { \s -> IdentifierT s }
{

data Token = ModuleDirectiveT 
           | ImportDirectiveT
           | TokensDirectiveT
           | GrammarDirectiveT 
           | UnderscoreT 
           | CurlyLBraceT
           | CurlyRBraceT
           | SquareLBraceT
           | SquareRBraceT
           | CommaT
           | ArrowT
           | DelimiterT
           | RefT Int
           | IdentifierT String
           deriving Show
}
