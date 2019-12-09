module ParserGenerator where

import TempLexer as TempLexer
import TemplateParser as TemplateParser
import TemplateLexer as TemplateLexer
import Data.Maybe
import Data.Bifunctor 
import TemplateGrammar
import DFA
import Control.Monad
import LexerGenerator


exec :: String -> IO ()
exec filename = do
  (content, newFilename) <- runProgram filename
  writeFile (newFilename ++ ".hs") content


runProgram :: String -> IO (String, String)
runProgram filename = do
  file <- readFile filename
  let (File parsed) = parseSLRFile $ TemplateLexer.alexScanTokens file
  let (content, modulename) = processStatements parsed
  return (content, modulename)


parseFile :: IO ()
parseFile = do
  content <- runProgram "src/SourceGrammar.slr"
  putStrLn $ fst content

processStatements :: [Statement] -> (String, String)
processStatements list = 
  let moduleStatement = extractModuleStatement list
      inlineStatements = extractInlineStatements list
      tokensStatement = extractTokensStatement list
      grammarStatement = extractGrammarStatements list
      attributes = extractAttributes list
      properTokens = extractProperTokens tokensStatement
      (header, modulename) = generateModuleHeader moduleStatement
      inlines = generateInlineCode inlineStatements >>= (++"\n")
      properGrammar = extractProperGrammar grammarStatement
      lexer = generateLexer properTokens
      body = buildDFA attributes properGrammar 
  in (header ++ "\n\n\n" ++ inlines ++ preimports ++ "\n\n\n" ++ lexer ++ "\n\n\n" ++ body, modulename)


preimports :: String
preimports = unlines [
            "import Control.Monad.State.Strict",
            "import Data.Bifunctor",
            "import System.IO.Unsafe",
            "import Control.Applicative",
            "import Text.Regex.PCRE",
            "\n\n\n",
            "type SLRState = State ([Int], [(String, String)])"]

extractModuleStatement :: [Statement] -> [Statement]
extractModuleStatement = mapMaybe (\s -> case s of 
  ModuleStatement _ -> Just s
  _ -> Nothing)  

extractInlineStatements :: [Statement] -> [Statement]
extractInlineStatements = mapMaybe (\s -> case s of
  InlineStatement _ -> Just s
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

generateModuleHeader :: [Statement] -> (String, String)
generateModuleHeader list = bimap ("{-# LANGUAGE PartialTypeSignatures #-}\n"++) id $
   case list of
    [ModuleStatement header] -> ("module " ++ header ++ " where", header)


generateInlineCode :: [Statement] -> [String]
generateInlineCode = fmap (\(InlineStatement s) -> tail $ take (length s - 1) s)


extractAttributes :: [Statement] -> [String]
extractAttributes [] = []
extractAttributes (e : tail) = case e of
    AttributesStatement list -> list ++ extractAttributes tail
    _ -> extractAttributes tail

