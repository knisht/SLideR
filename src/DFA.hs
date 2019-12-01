module DFA where

{-# LANGUAGE PartialTypeSignatures #-}

import TemplateGrammar
import Data.Set as Set
import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Maybe
import Data.Bifunctor
import Data.List as List
import Data.Maybe as Maybe
import Debug

type IndexedState = (String, [String], [String])

type RuleMap = Map String [[String]]

type Config = [IndexedState]

type Context a = State RuleMap a

type ConfigurationMap = Map Int Config 

type Transition = (Int, TransitionType, Int)

data TransitionType = Shift String | Reduce deriving (Show, Eq, Ord)

type TransitionMap = Map (Int, TransitionType) Int

type DFAContext a = State (TransitionMap, ConfigurationMap, RuleMap) a

buildDFA :: [Terminal] -> [RuleDefinition] -> String
buildDFA terminals ruleDefs = 
  let (RuleDefinition oldInitial _) = head ruleDefs
      initial = RuleDefinition generateNewInitialState [RuleAction [oldInitial] [Ref 0]]
      newList = initial : ruleDefs
      nonterminals = collectNonterminals (initial : ruleDefs)
      ruleMap = Map.insert generateNewInitialState [[oldInitial]] $ buildRuleMap ruleDefs      
      (transits, configs, rules) = execState (buildCompleteTransitionTable initial) (Map.empty, Map.empty, ruleMap)
      functions = generateFunctions transits configs rules
  in intercalate "\n\n\n" functions



collectNonterminals :: [RuleDefinition] -> Set String
collectNonterminals = Prelude.foldr (\(RuleDefinition s _) set -> Set.insert s set) Set.empty


generateNewInitialState :: String 
generateNewInitialState = "S'"

closure :: [IndexedState] -> Context Config 
closure istates = do
  map <- get
  return $ evalState (join <$> traverse closureHelper istates) (map, Set.empty)

closureHelper :: IndexedState -> State (RuleMap, Set IndexedState) Config
closureHelper st@(var, left, right) = do
  (_, visited) <- get
  if Set.member st visited then return []
  else case right of
    [] -> do return [st]
    a : _ -> do
      modify (bimap id (Set.insert st))
      isTerm <- isTerminal a
      --yield isTerm 
      if isTerm then return [st] else do
        startIndexedStates <- getStartIndexedStates a
        innerStates <- join <$> traverse (closureHelper) startIndexedStates
        return $ st : innerStates


isTerminal :: String -> State (RuleMap, Set IndexedState) Bool
isTerminal s = do
  (map, _) <- get
  return $ isNothing (map Map.!? s)


getStartIndexedStates :: String -> State (RuleMap, Set IndexedState) [IndexedState]
getStartIndexedStates s = do
  (map, _) <- get
  let list = map Map.! s
  let istates = (\rule -> (s, [], rule)) <$> list
  return istates


safeHead :: [a] -> Maybe a
safeHead list = case list of
  [] -> Nothing
  a : _ -> Just a

generateShiftTransition :: Int -> String -> DFAContext (Maybe Transition)
generateShiftTransition ind symbol = do
  defConfig <- getConfig ind
  let defaultConfig = defConfig
  rules <- getRules
  let suitableRules = Prelude.filter (\(_, _, right) -> isJust $ safeHead right >>= (\x -> if x == symbol then Just True else Nothing)) defaultConfig 
  let newConfiguration = evalState (closure (moveCaret suitableRules)) rules
  alreadyInMap <- findConfiguration newConfiguration
  {- slow -}
  newIndex <- if alreadyInMap == -1 then genNumber else return alreadyInMap
  let newTransition = (ind, Shift symbol, newIndex)
  --yield newIndex
  modify (\(a, b, c) -> (Map.insert (ind, Shift symbol) newIndex a, (Map.insert newIndex newConfiguration b), c))
  return $ if alreadyInMap /= -1 then Nothing else Just newTransition

getConfig :: Int -> DFAContext Config
getConfig i = (Map.! i) <$> getCMap

moveCaret :: Config -> Config
moveCaret confs = Maybe.mapMaybe moveSingle confs

moveSingle :: IndexedState -> Maybe IndexedState
moveSingle (p, l, r) = case r of
  [] -> Nothing
  a : tail -> Just (p, a : l, tail)

getCMap :: DFAContext ConfigurationMap
getCMap = (\(_, a, _) -> a) <$> get

getRules :: DFAContext RuleMap
getRules = (\(_,_,a) -> a) <$> get

getTransitions :: DFAContext TransitionMap
getTransitions = (\(a, _, _) -> a) <$> get

genNumber :: DFAContext Int
genNumber = Map.size <$> getCMap

findConfiguration :: Config -> DFAContext Int
findConfiguration c = do
  map <- getCMap
  let listValues = Map.toList map
  let suitableConfigs = Prelude.filter ((==) c . snd) listValues  
  case suitableConfigs of
    [(ind, _)] -> return ind
    _ -> return (-1)


buildTransitionTable :: Int -> DFAContext ()
buildTransitionTable configIndex = do
  --yield ("MyIndex", configIndex)
  config <- getConfig configIndex  
  yield (configIndex, config)
  let availableSymbols = getAvailableSymbols config 
  newStatesUnwrapped <- traverse (generateShiftTransition configIndex) availableSymbols
  let newStates = Maybe.mapMaybe id newStatesUnwrapped
  --yield newStates
  generateReduceTransition configIndex
  let newStateIndices = (\(_, _, i) -> i) <$> newStates
  --yield ("indices: ", newStateIndices)
  mapM_ buildTransitionTable newStateIndices
  return ()


generateReduceTransition :: Int -> DFAContext ()
generateReduceTransition configIndex = do
  config <- getConfig configIndex
  searchResult <- findFinishTransition config
  case searchResult of
    Just num -> modify (\(a, b, c) -> (Map.insert (configIndex, Reduce) num a, b, c))
    Nothing -> return ()


findFinishTransition :: Config -> DFAContext (Maybe Int)
findFinishTransition config = do
  let endpoints = List.filter (\(_, _, r) -> List.null r) config
  case endpoints of
    [(e, l, [])] -> do 
      rules <- getRules
      --yield config
      eRules <- return $ rules Map.! e
      let [exactRule] = List.filter ((==) (reverse l) . fst) (zip eRules ([1..] :: [Int]))
      (return . Just . snd) exactRule
    _ -> return Nothing
  

getAvailableSymbols :: Config -> [String]
getAvailableSymbols = List.nub . (Maybe.mapMaybe (\(_, _, r) -> safeHead r)) 


buildCompleteTransitionTable :: RuleDefinition -> DFAContext ()
buildCompleteTransitionTable (RuleDefinition s [(RuleAction [a] _)]) = do
  let indexed = (s, [], [a])
  rules <- getRules
  let newConfiguration = evalState (closure [indexed]) rules
  --yield newConfiguration
  modify ((\(x, y, z) -> (x, Map.insert 0 newConfiguration y, z )))
  buildTransitionTable 0
  return ()



buildRuleMap :: [RuleDefinition] -> RuleMap
buildRuleMap = Map.fromList . (fmap (\(RuleDefinition s acts) -> (s, (\(RuleAction a _) -> a) <$> acts)))


generateFunctions :: TransitionMap -> ConfigurationMap -> RuleMap -> [String]
generateFunctions transitions configs rules = 
  let possibleWays = (\((a, b), c) -> (a, b, c)) <$> Map.toList transitions
      splitted = groupBy (\(a, _, _) (b, _, _) -> a == b) possibleWays
  in
  splitted ? renderFunctionDef <$> splitted



renderFunctionDef :: [(Int, TransitionType, Int)] -> String
renderFunctionDef list =
  let (nameInd, _, _) = head list
      functionName = "slrUnit_" ++ show nameInd 
      functionType = functionName ++ " :: SLRState _"
      functionHeader = functionName ++ " = do"
      bodyPrefix = "  modify (bimap ((:) " ++ show nameInd ++ ") id)\n  currentSymbol <- safeHead <$> snd <$> get"
      caseHeader = "  case currentSymbol of"
      caseStatements = (\(_, t, i) -> generateCaseStatement t i) <$> list
  in intercalate "\n" ([functionType, functionHeader, bodyPrefix, caseHeader] ++ caseStatements)


generateCaseStatement :: TransitionType -> Int -> String
generateCaseStatement ttype target = case ttype of
  Shift s -> "    Just " ++ show s ++ " -> slrUnit_" ++ show target
  Reduce ->  "    Nothing -> do {-remove states-} slrDispatcher"
