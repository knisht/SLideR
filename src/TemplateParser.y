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
    ATTRIBUTESDIR { AttributesDirectiveT }
    IDENTIFIER  { IdentifierT $$    }
    CODEBLOCK   { CodeBlockT $$     } 
    REGEX       { RegexT $$         }
    COMMA       { CommaT            }
    ARROW       { ArrowT            }
    DELIMITER   { DelimiterT        }
    ATTRLBRACE  { AttrLBraceT       }
    ATTRRBRACE  { AttrRBraceT       }
    EQSIGN      { EqSignT           }


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
    | ATTRIBUTESDIR AttributesList { AttributesStatement $2}


AttributesList
    : IDENTIFIER { [$1] }
    | AttributesList IDENTIFIER { $1 ++ [$2] }

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
    : CaseList ATTRLBRACE ActionList ATTRRBRACE  { RuleAction $1 $3 }

CaseList 
    : CaseList IDENTIFIER { $1 ++ [$2] }
    | {- empty -}       { [] }

ActionList
    : Assign                { [$1] }
    | ActionList COMMA Assign     {$1 ++ [$3]}

Assign
    : IDENTIFIER EQSIGN CODEBLOCK  { Assign $1 $3 }

{
parseError = fail "ParseError"
}
