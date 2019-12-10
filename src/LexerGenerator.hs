module LexerGenerator where


import TemplateGrammar
import Data.List


generateLexer :: [Terminal] -> String
generateLexer terminals = intercalate "\n\n" $ 
  generateLexingAnalyzer : 
  generateCompositeLexer terminals :
  generateGenericLexer : 
  (generateSingleLexer <$> terminals)


generateGenericLexer :: String
generateGenericLexer = unlines [
 "runLex :: String -> String -> String -> Maybe (String, String, String)",
 "runLex regex name s = let splitted = ((s =~~ regex) :: Maybe (String, String, String, [String]))   in",
 "   case splitted of",
 "     Just (\"\", matched, after, _) -> Just (name, matched, after)",
 "     _ -> Nothing"]


generateSingleLexer :: Terminal -> String
generateSingleLexer (Terminal name regex) = unlines 
  ["lex" ++ name ++ " :: String -> Maybe (String, String, String)",
   "lex" ++ name ++ " = runLex " ++ show (unwrapRegex regex) ++ " " ++ show name]


generateCompositeLexer :: [Terminal] -> String
generateCompositeLexer list = "compositeLexer s = " ++ 
  (intercalate " <|> " $ (\(Terminal name _) -> "lex" ++ name ++ " s") <$> list)


generateLexingAnalyzer :: String
generateLexingAnalyzer = unlines
  ["slrLexer :: String -> [(String, String)]",
   "slrLexer s = case compositeLexer s of",
   "  Nothing -> if (Prelude.null s) then [] else undefined",
   "  Just (name, parsed, remained) -> if name == \"_\" then slrLexer remained else (name, parsed) : slrLexer remained"]


unwrapRegex :: String -> String
unwrapRegex ('"' : left) = '^' : take (length left - 2) left

