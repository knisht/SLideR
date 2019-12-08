module SampleGrammar where


import Grammar
{-# LANGUAGE PartialTypeSignatures #-}
import Control.Monad.State.Strict
import Data.Bifunctor
import System.IO.Unsafe
import Control.Applicative
import Text.Regex




type SLRState = State ([Int], [(String, String)])



slrLexer :: String -> [(String, String)]
slrLexer s = case compositeLexer s of
  Nothing -> []
  Just (name, parsed, remained) -> if name == "_" then slrLexer remained else (name, parsed) : slrLexer remained


compositeLexer s = lex_ s <|> lexPlusT s <|> lexMultT s <|> lexVarT s <|> lexLBraceT s <|> lexRBraceT s

lex_ :: String -> Maybe (String, String, String)
lex_ s = 
  let regex = mkRegex "^ |\\n"
      splitted = splitRegex regex s in
  case splitted of
    [a, b] -> Just ("_", Prelude.take (length s - length b) s, b)
    _ -> Nothing


lexPlusT :: String -> Maybe (String, String, String)
lexPlusT s = 
  let regex = mkRegex "^\\+"
      splitted = splitRegex regex s in
  case splitted of
    [a, b] -> Just ("PlusT", Prelude.take (length s - length b) s, b)
    _ -> Nothing


lexMultT :: String -> Maybe (String, String, String)
lexMultT s = 
  let regex = mkRegex "^\\*"
      splitted = splitRegex regex s in
  case splitted of
    [a, b] -> Just ("MultT", Prelude.take (length s - length b) s, b)
    _ -> Nothing


lexVarT :: String -> Maybe (String, String, String)
lexVarT s = 
  let regex = mkRegex "^[0-9]+"
      splitted = splitRegex regex s in
  case splitted of
    [a, b] -> Just ("VarT", Prelude.take (length s - length b) s, b)
    _ -> Nothing


lexLBraceT :: String -> Maybe (String, String, String)
lexLBraceT s = 
  let regex = mkRegex "^\\("
      splitted = splitRegex regex s in
  case splitted of
    [a, b] -> Just ("LBraceT", Prelude.take (length s - length b) s, b)
    _ -> Nothing


lexRBraceT :: String -> Maybe (String, String, String)
lexRBraceT s = 
  let regex = mkRegex "^\\)"
      splitted = splitRegex regex s in
  case splitted of
    [a, b] -> Just ("RBraceT", Prelude.take (length s - length b) s, b)
    _ -> Nothing



mainEvaluator input = getNValue1 $ head $ evalState (slrDispatcher []) ([0], slrLexer input)


data ValueContainer t1 t2 t3 t4 = Dummy
  | Success
  | TokenValue String
  | NontermValue1 t1
  | NontermValue2 t2
  | NontermValue3 t3
  | NontermValue4 t4
  deriving Show


getNValue1 (NontermValue1 a) = a
getNValue2 (NontermValue2 a) = a
getNValue3 (NontermValue3 a) = a
getNValue4 (NontermValue4 a) = a
getTokenValue (TokenValue s) = s


getAttr_value (t, _) = t

getAttr_repr (_, t) = t


isSuccess e = case e of
  Success -> True
  _ -> False



slrStateStackPeek :: SLRState Int
slrStateStackPeek = head <$> fst <$> get


restoreTokenStack s = case s of
  Nothing -> return ()
  Just a -> slrTokenStackPush a



slrDispatcher valueStack = do
  lastState <- slrStateStackPeek
  (producer, numToPop) <- case lastState of
    0 -> slrUnit_0
    1 -> slrUnit_1
    2 -> slrUnit_2
    3 -> slrUnit_3
    4 -> slrUnit_4
    5 -> slrUnit_5
    6 -> slrUnit_6
    7 -> slrUnit_7
    8 -> slrUnit_8
    9 -> slrUnit_9
    10 -> slrUnit_10
    11 -> slrUnit_11
  let (top, tail) = Prelude.splitAt numToPop valueStack
  if numToPop == -1 then slrDispatcher valueStack else do
    let newValue = (producer top)
    if isSuccess newValue then return valueStack else slrDispatcher (newValue : tail)



slrTokenStackPop = do
  stack <- snd <$> get
  if Prelude.null stack then do
    return Nothing
  else do
    let hd = head stack
    modify $ bimap id tail
    return $ Just hd



slrTokenStackPush tokenPair = do
  modify $ bimap id ((:) tokenPair)



slrStateStackPush ind = do
  modify $ bimap ((:) ind) id



slrStateStackPop = do
  hd <- head <$> fst <$> get
  modify $ bimap tail id
  return hd



slrUnit_0 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("E", val) -> do slrStateStackPush 1; return (\[] -> TokenValue val, -1)
    Just ("F", val) -> do slrStateStackPush 3; return (\[] -> TokenValue val, -1)
    Just ("LBraceT", val) -> do slrStateStackPush 4; return (\[] -> TokenValue val, 0)
    Just ("T", val) -> do slrStateStackPush 2; return (\[] -> TokenValue val, -1)
    Just ("VarT", val) -> do slrStateStackPush 5; return (\[] -> TokenValue val, 0)




slrUnit_1 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("PlusT", val) -> do slrStateStackPush 6; return (\[] -> TokenValue val, 0)
    Nothing -> return (\[] -> Success, 0)




slrUnit_2 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("MultT", val) -> do slrStateStackPush 8; return (\[] -> TokenValue val, 0)
    Nothing -> slrReduceHelper_2 pair
    Just ("PlusT", _) -> slrReduceHelper_2 pair
    Just ("RBraceT", _) -> slrReduceHelper_2 pair

slrReduceHelper_2 :: Maybe (String, String) -> SLRState _
slrReduceHelper_2 pair = do 
  restoreTokenStack pair
  slrStateStackPop
  slrTokenStackPush ("E", "UNDEFINED")
  return (\[t0] -> NontermValue1 (((getAttr_value (getNValue4 t0)), (getAttr_repr (getNValue4 t0)))), 1)


slrUnit_3 = do
  pair <- slrTokenStackPop

  case pair of
    Nothing -> slrReduceHelper_3 pair
    Just ("MultT", _) -> slrReduceHelper_3 pair
    Just ("PlusT", _) -> slrReduceHelper_3 pair
    Just ("RBraceT", _) -> slrReduceHelper_3 pair

slrReduceHelper_3 :: Maybe (String, String) -> SLRState _
slrReduceHelper_3 pair = do 
  restoreTokenStack pair
  slrStateStackPop
  slrTokenStackPush ("T", "UNDEFINED")
  return (\[t0] -> NontermValue4 (((getAttr_value (getNValue2 t0)), (getAttr_repr (getNValue2 t0)))), 1)


slrUnit_4 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("E", val) -> do slrStateStackPush 10; return (\[] -> TokenValue val, -1)
    Just ("F", val) -> do slrStateStackPush 3; return (\[] -> TokenValue val, -1)
    Just ("LBraceT", val) -> do slrStateStackPush 4; return (\[] -> TokenValue val, 0)
    Just ("T", val) -> do slrStateStackPush 2; return (\[] -> TokenValue val, -1)
    Just ("VarT", val) -> do slrStateStackPush 5; return (\[] -> TokenValue val, 0)




slrUnit_5 = do
  pair <- slrTokenStackPop

  case pair of
    Nothing -> slrReduceHelper_5 pair
    Just ("MultT", _) -> slrReduceHelper_5 pair
    Just ("PlusT", _) -> slrReduceHelper_5 pair
    Just ("RBraceT", _) -> slrReduceHelper_5 pair

slrReduceHelper_5 :: Maybe (String, String) -> SLRState _
slrReduceHelper_5 pair = do 
  restoreTokenStack pair
  slrStateStackPop
  slrTokenStackPush ("F", "UNDEFINED")
  return (\[t0] -> NontermValue2 (((read (getTokenValue t0)) :: Int, (getTokenValue t0))), 1)


slrUnit_6 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("F", val) -> do slrStateStackPush 3; return (\[] -> TokenValue val, -1)
    Just ("LBraceT", val) -> do slrStateStackPush 4; return (\[] -> TokenValue val, 0)
    Just ("T", val) -> do slrStateStackPush 7; return (\[] -> TokenValue val, -1)
    Just ("VarT", val) -> do slrStateStackPush 5; return (\[] -> TokenValue val, 0)




slrUnit_7 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("MultT", val) -> do slrStateStackPush 8; return (\[] -> TokenValue val, 0)
    Nothing -> slrReduceHelper_7 pair
    Just ("PlusT", _) -> slrReduceHelper_7 pair
    Just ("RBraceT", _) -> slrReduceHelper_7 pair

slrReduceHelper_7 :: Maybe (String, String) -> SLRState _
slrReduceHelper_7 pair = do 
  restoreTokenStack pair
  slrStateStackPop
  slrStateStackPop
  slrStateStackPop
  slrTokenStackPush ("E", "UNDEFINED")
  return (\[t2, t1, t0] -> NontermValue1 (((getAttr_value (getNValue1 t0)) + (getAttr_value (getNValue4 t2)), (getAttr_repr (getNValue1 t0)))), 3)


slrUnit_8 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("F", val) -> do slrStateStackPush 9; return (\[] -> TokenValue val, -1)
    Just ("LBraceT", val) -> do slrStateStackPush 4; return (\[] -> TokenValue val, 0)
    Just ("VarT", val) -> do slrStateStackPush 5; return (\[] -> TokenValue val, 0)




slrUnit_9 = do
  pair <- slrTokenStackPop

  case pair of
    Nothing -> slrReduceHelper_9 pair
    Just ("MultT", _) -> slrReduceHelper_9 pair
    Just ("PlusT", _) -> slrReduceHelper_9 pair
    Just ("RBraceT", _) -> slrReduceHelper_9 pair

slrReduceHelper_9 :: Maybe (String, String) -> SLRState _
slrReduceHelper_9 pair = do 
  restoreTokenStack pair
  slrStateStackPop
  slrStateStackPop
  slrStateStackPop
  slrTokenStackPush ("T", "UNDEFINED")
  return (\[t2, t1, t0] -> NontermValue4 (((getAttr_value (getNValue4 t0)) * (getAttr_value (getNValue2 t2)), (getAttr_repr (getNValue4 t0)))), 3)


slrUnit_10 = do
  pair <- slrTokenStackPop

  case pair of
    Just ("PlusT", val) -> do slrStateStackPush 6; return (\[] -> TokenValue val, 0)
    Just ("RBraceT", val) -> do slrStateStackPush 11; return (\[] -> TokenValue val, 0)




slrUnit_11 = do
  pair <- slrTokenStackPop

  case pair of
    Nothing -> slrReduceHelper_11 pair
    Just ("MultT", _) -> slrReduceHelper_11 pair
    Just ("PlusT", _) -> slrReduceHelper_11 pair
    Just ("RBraceT", _) -> slrReduceHelper_11 pair

slrReduceHelper_11 :: Maybe (String, String) -> SLRState _
slrReduceHelper_11 pair = do 
  restoreTokenStack pair
  slrStateStackPop
  slrStateStackPop
  slrStateStackPop
  slrTokenStackPush ("F", "UNDEFINED")
  return (\[t2, t1, t0] -> NontermValue2 (((getAttr_value (getNValue1 t1)), (getAttr_repr (getNValue1 t1)))), 3)