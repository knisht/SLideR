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
    "%attributes"                       { \_ -> AttributesDirectiveT }
    \-\>                                { \_ -> ArrowT }
    \|                                  { \_ -> DelimiterT }
    \,                                  { \_ -> CommaT}
    [$alpha \_] [$alpha $digit]*        { \s -> IdentifierT s }
    \[                                  { \_ -> AttrLBraceT }
    \]                                  { \_ -> AttrRBraceT }
    =                                   { \_ -> EqSignT }
    \{ ([^\}]+) \}                      { \s -> CodeBlockT s}
    \"(.+)\"\n                         { \s -> RegexT s }
{

data Token = ModuleDirectiveT 
           | ImportDirectiveT
           | TokensDirectiveT
           | GrammarDirectiveT 
           | AttributesDirectiveT
           | CommaT
           | AttrLBraceT
           | AttrRBraceT
           | ArrowT
           | EqSignT
           | DelimiterT
           | CodeBlockT String
           | RegexT String
           | IdentifierT String
           deriving Show
}
