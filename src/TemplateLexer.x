{
module TemplateLexer where
}

%wrapper "basic"

$digit      = 0-9
$alpha      = [a-zA-Z]

tokens :-
    $white+                             ;
    "%module"                           { \_ -> ModuleDirectiveT }
    "%import"                           { \_ -> ImportDirectiveT }
    "%tokens"                           { \_ -> TokensDirectiveT }
    "%grammar"                          { \_ -> GrammarDirectiveT }
    \{                                  { \_ -> CurlyLBraceT }
    \}                                  { \_ -> CurlyRBraceT }
    \-\>                                { \_ -> ArrowT }
    \|                                  { \_ -> DelimiterT }
    \,                                  { \_ -> CommaT}
    \$ [$digit]+                        { \s -> RefT $ read (tail s) }                 
    [$alpha \_] [$alpha $digit]*             { \s -> IdentifierT s }
    \"(.+)?\"\n                         { \s -> RegexT s }
{

data Token = ModuleDirectiveT 
           | ImportDirectiveT
           | TokensDirectiveT
           | GrammarDirectiveT 
           | UnderscoreT 
           | CurlyLBraceT
           | CurlyRBraceT
           | CommaT
           | ArrowT
           | DelimiterT
           | RegexT String
           | RefT Int
           | IdentifierT String
           deriving Show
}
