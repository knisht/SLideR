module SampleGrammar where


import TempLexer
import Grammar
{-# LANGUAGE PartialTypeSignatures #-}
import Control.Monad.State.Strict
import Data.Bifunctor
import System.IO.Unsafe
import Debug




type SLRState = State ([Int], [(String, String)])



data ValueContainer t1 t2 t3 = Dummy
  | Success
  | TokenValue String
  | NontermValue1 t1
  | NontermValue2 t2
  | NontermValue3 t3
  deriving Show


getNValue1 (NontermValue1 a) = a
getNValue2 (NontermValue2 a) = a
getNValue3 (NontermValue3 a) = a
getTokenValue (TokenValue s) = s


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
  let (top, tail) = splitAt numToPop valueStack
  if numToPop == -1 then slrDispatcher valueStack else do
    let newValue = (producer top)
    if isSuccess newValue then return valueStack else slrDispatcher (newValue : tail)



slrTokenStackPop = do
  stack <- snd <$> get
  if null stack then do
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
  yield 0 
  case pair of
    Just ("E", val) -> do slrStateStackPush 1; return (\[] -> TokenValue val, -1)
    Just ("LBraceT", val) -> do slrStateStackPush 3; return (\[] -> TokenValue val, 0)
    Just ("T", val) -> do slrStateStackPush 2; return (\[] -> TokenValue val, -1)
    Just ("VarT", val) -> do slrStateStackPush 4; return (\[] -> TokenValue val, 0)


slrUnit_1 = do
  pair <- slrTokenStackPop
  yield 1 
  case pair of
    Just ("PlusT", val) -> do slrStateStackPush 5; return (\[] -> TokenValue val, 0)
    Nothing -> return (\[] -> Success, 0)


slrUnit_2 = do
  pair <- slrTokenStackPop
  yield 2 
  case pair of
    _ -> do 
      restoreTokenStack pair
      slrStateStackPop
      slrTokenStackPush ("E", "UNDEFINED")
      return (\[t0] -> NontermValue1 $ (getNValue3 t0) , 1)


slrUnit_3 = do
  pair <- slrTokenStackPop
  yield 3 
  case pair of
    Just ("E", val) -> do slrStateStackPush 7; return (\[] -> TokenValue val, -1)
    Just ("LBraceT", val) -> do slrStateStackPush 3; return (\[] -> TokenValue val, 0)
    Just ("T", val) -> do slrStateStackPush 2; return (\[] -> TokenValue val, -1)
    Just ("VarT", val) -> do slrStateStackPush 4; return (\[] -> TokenValue val, 0)


slrUnit_4 = do
  pair <- slrTokenStackPop
  yield 4 
  case pair of
    _ -> do 
      restoreTokenStack pair
      slrStateStackPop
      slrTokenStackPush ("T", "UNDEFINED")
      return (\[t0] -> NontermValue3 $ Var (getTokenValue t0) , 1)


slrUnit_5 = do
  pair <- slrTokenStackPop
  yield 5 
  case pair of
    Just ("LBraceT", val) -> do slrStateStackPush 3; return (\[] -> TokenValue val, 0)
    Just ("T", val) -> do slrStateStackPush 6; return (\[] -> TokenValue val, -1)
    Just ("VarT", val) -> do slrStateStackPush 4; return (\[] -> TokenValue val, 0)


slrUnit_6 = do
  pair <- slrTokenStackPop
  yield 6 
  case pair of
    _ -> do 
      restoreTokenStack pair
      slrStateStackPop
      slrStateStackPop
      slrStateStackPop
      slrTokenStackPush ("E", "UNDEFINED")
      return (\[t2, t1, t0] -> NontermValue1 $ Add (getNValue1 t0) (getNValue3 t2) , 3)


slrUnit_7 = do
  pair <- slrTokenStackPop
  yield 7 
  case pair of
    Just ("PlusT", val) -> do slrStateStackPush 5; return (\[] -> TokenValue val, 0)
    Just ("RBraceT", val) -> do slrStateStackPush 8; return (\[] -> TokenValue val, 0)


slrUnit_8 = do
  pair <- slrTokenStackPop
  yield 8 
  case pair of
    _ -> do 
      restoreTokenStack pair
      slrStateStackPop
      slrStateStackPop
      slrStateStackPop
      slrTokenStackPush ("T", "UNDEFINED")
      return (\[t2, t1, t0] -> NontermValue3 $ (getNValue1 t1) , 3)
