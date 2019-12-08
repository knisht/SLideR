module TemplateGrammar where 


data File = File [Statement] deriving Show

data Statement = ModuleStatement  String 
               | ImportStatement  String
               | TokensStatement  [Terminal]
               | GrammarStatement [RuleDefinition]
               | AttributesStatement [String]
               deriving Show

data Terminal = Terminal String String
              deriving Show    
            
data RuleDefinition = RuleDefinition String [RuleAction] deriving Show

data RuleAction = RuleAction [String] [Assign] deriving Show

data Assign = Assign String String
             deriving Show

