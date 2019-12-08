module LexerGenerator where


import TemplateGrammar
import Data.List


generateLexer :: [Terminal] -> String
generateLexer terminals = intercalate "\n\n" $ 
  generateLexingAnalyzer : 
  generateCompositeLexer terminals :
  (generateSingleLexer <$> terminals)


generateSingleLexer :: Terminal -> String
generateSingleLexer (Terminal name regex) = unlines 
  ["lex" ++ name ++ " :: String -> Maybe (String, String, String)",
   "lex" ++ name ++ " s = ",
   "  let regex = mkRegex " ++ show (unwrapRegex regex),
   "      splitted = splitRegex regex s in",
   "  case splitted of",
   "    [a, b] -> Just (" ++ show name ++ ", take (length s - length b) s, b)",
   "    _ -> Nothing"]


generateCompositeLexer :: [Terminal] -> String
generateCompositeLexer list = "compositeLexer s = " ++ 
  (intercalate " <|> " $ (\(Terminal name _) -> "lex" ++ name ++ " s") <$> list)


generateLexingAnalyzer :: String
generateLexingAnalyzer = unlines
  ["slrLexer :: String -> [(String, String)]",
   "slrLexer s = case compositeLexer s of",
   "  Nothing -> []",
   "  Just (name, parsed, remained) -> if name == \"_\" then slrLexer remained else (name, parsed) : slrLexer remained"]


unwrapRegex :: String -> String
unwrapRegex ('"' : left) = '^' : take (length left - 2) left

