module TemplateGrammar where 


data File = File [Statement] deriving Show

data Statement = ModuleStatement  String 
               | ImportStatement  String
               | TokensStatement  [Terminal]
               | GrammarStatement [RuleDefinition]
               deriving Show

data Terminal = TokenTerminal String
              | ArgumentTerminal String 
              deriving Show    
            
data RuleDefinition = RuleDefinition String [RuleAction] deriving Show

data RuleAction = RuleAction [String] [NameOrRef] deriving Show

data NameOrRef = Name String
               | Ref Int
               deriving Show

