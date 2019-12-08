{
module TemplateParser where


import TemplateLexer
import TemplateGrammar
}

%name parseSLRFile
%tokentype { Token }
%error { parseError }

%token
    MODULEDIR   { ModuleDirectiveT  }
    IMPORTDIR   { ImportDirectiveT  }
    TOKENSDIR   { TokensDirectiveT  }
    GRAMMARDIR  { GrammarDirectiveT }
    REF         { RefT $$           }
    IDENTIFIER  { IdentifierT $$    }
    CLBRACE     { CurlyLBraceT      }
    CRBRACE     { CurlyRBraceT      }
    REGEX       { RegexT $$         }
    ARROW       { ArrowT            }
    DELIMITER   { DelimiterT        }
    


%%

File 
    : StatementList    { File $1 }

StatementList
    : Statement        { [$1] }
    | StatementList Statement        { $1 ++ [$2] }

Statement 
    : MODULEDIR IDENTIFIER   { ModuleStatement $2 }
    | IMPORTDIR IDENTIFIER   { ImportStatement $2 }
    | TOKENSDIR TerminalList { TokensStatement $2 }
    | GRAMMARDIR GrammarList { GrammarStatement $2}

TerminalList 
    : Terminal { [$1] }
    | TerminalList Terminal {$1 ++ [$2]}

Terminal 
    : IDENTIFIER REGEX       { Terminal $1 $2 }
    
GrammarList
    : RuleDefinition                 { [$1] }
    | GrammarList RuleDefinition { $1 ++ [$2]}

RuleDefinition
    : IDENTIFIER ARROW RuleAction RuleList { RuleDefinition $1 ($3 : $4 ) }

RuleList 
    : RuleList DELIMITER RuleAction { $1 ++ [$3] }
    | {- empty -}          { [] }

RuleAction 
    : CaseList CLBRACE ActionList CRBRACE  { RuleAction $1 $3 }

CaseList 
    : CaseList IDENTIFIER { $1 ++ [$2] }
    | {- empty -}       { [] }

ActionList
    : ActionList NameOrRef {$1 ++ [$2]}
    | {-empty-}     { [] }

NameOrRef
    : IDENTIFIER    {Name $1}
    | REF           {Ref $1}

{
parseError = fail "ParseError"
}
