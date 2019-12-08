module ParserGenerator where

import TempLexer as TempLexer
import TemplateParser as TemplateParser
import TemplateLexer as TemplateLexer
import Data.Maybe
import TemplateGrammar
import DFA
import Control.Monad
import LexerGenerator

str = "%module Sample"

parseFile :: IO ()
parseFile = do
  file <- readFile "src/SourceGrammar.slr"
  let (File parsed) = parseSLRFile $ TemplateLexer.alexScanTokens file
  let content = processStatements parsed
  putStrLn content

processStatements :: [Statement] -> String
processStatements list = 
  let moduleStatement = extractModuleStatement list
      importStatements = extractImportStatements list
      tokensStatement = extractTokensStatement list
      grammarStatement = extractGrammarStatements list
      properTokens = extractProperTokens tokensStatement
      header = generateModuleHeader moduleStatement
      imports = generateImportHeader importStatements >>= (++"\n")
      properGrammar = extractProperGrammar grammarStatement
      lexer = generateLexer properTokens
      body = buildDFA properTokens properGrammar 
  in header ++ "\n\n\n" ++ imports ++ preimports ++ "\n\n\n" ++ lexer ++ "\n\n\n" ++ body


preimports :: String
preimports = unlines ["{-# LANGUAGE PartialTypeSignatures #-}",
            "import Control.Monad.State.Strict",
            "import Data.Bifunctor",
            "import System.IO.Unsafe",
            "import Control.Applicative",
            "import Text.Regex",
            "import Debug",
            "\n\n\n",
            "type SLRState = State ([Int], [(String, String)])"]

extractModuleStatement :: [Statement] -> [Statement]
extractModuleStatement = mapMaybe (\s -> case s of 
  ModuleStatement _ -> Just s
  _ -> Nothing)  

extractImportStatements :: [Statement] -> [Statement]
extractImportStatements = mapMaybe (\s -> case s of
  ImportStatement _ -> Just s
  _ -> Nothing)

extractTokensStatement :: [Statement] -> [Statement]
extractTokensStatement = mapMaybe (\s -> case s of
  TokensStatement _ -> Just s
  _ -> Nothing)

extractGrammarStatements :: [Statement] -> [Statement]
extractGrammarStatements = mapMaybe (\s -> case s of
  GrammarStatement _ -> Just s
  _ -> Nothing) 

              

extractProperTokens :: [Statement] -> [Terminal]
extractProperTokens = join . fmap (\(TokensStatement s) -> s)  

extractProperGrammar :: [Statement] -> [RuleDefinition]
extractProperGrammar = join . fmap (\(GrammarStatement s) -> s)

generateModuleHeader :: [Statement] -> String
generateModuleHeader list = case list of
  [ModuleStatement header] -> "module " ++ header ++ " where"


generateImportHeader :: [Statement] -> [String]
generateImportHeader = fmap (\(ImportStatement s) -> "import " ++ s)




