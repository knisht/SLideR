module DFA where

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}


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
type RuleMap = Map String [([String], [NameOrRef])]
type Config = [IndexedState]
type Context a = State RuleMap a
type ConfigurationMap = Map Int Config 
type Transition = (Int, TransitionType, Int)
data TransitionType = Shift String | Reduce String deriving (Show, Eq, Ord)
type TransitionMap = Map (Int, TransitionType) Int
type DFAContext a = State (TransitionMap, ConfigurationMap, RuleMap) a

infixr 9 ∘
(∘) = (.)


isDebugMode = True

buildDFA :: [Terminal] -> [RuleDefinition] -> String
buildDFA terminals ruleDefs = 
  let (RuleDefinition oldInitial _) = head ruleDefs
      initial = RuleDefinition generateNewInitialState [RuleAction [oldInitial] [Ref 0]]
      newList = initial : ruleDefs
      nonterminals = collectNonterminals (initial : ruleDefs)
      ruleMap = {-Map.insert generateNewInitialState [[oldInitial]] $-} buildRuleMap newList
      typeContainer = generateTypeContainer (length newList)
      typeGetters = generateTypeGetters (length newList)
      (transits, configs, rules) = execState (buildCompleteTransitionTable initial) (Map.empty, Map.empty, ruleMap)
      functions = generateFunctions transits configs rules
  in intercalate "\n\n\n" (typeContainer : typeGetters : functions)



generateTypeContainer :: Int -> String
generateTypeContainer number = intercalate "\n" $ 
  ("data ValueContainer " ++ 
  (intercalate " " $ (\i -> "t" ++ show i) <$> [1..number]) ++ " = Dummy") : 
  ("  | Success" : 
   "  | TokenValue String" : 
   ((\i -> "  | NontermValue" ++ show i ++ " t" ++ show i) <$> [1..number]) ++ ["  deriving Show"])


generateTypeGetters :: Int -> String
generateTypeGetters number = 
  intercalate "\n" $ 
    ((\i -> "getNValue" ++ show i ++ " (NontermValue" ++ show i ++ " a) = a") <$> [1..number])
  ++ ["getTokenValue (TokenValue s) = s"]

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
  let istates = (\(rule, _) -> (s, [], rule)) <$> list
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
  --yield (configIndex, config)
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
    Just (num, termname) -> modify (\(a, b, c) -> (Map.insert (configIndex, Reduce termname) num a, b, c))
    Nothing -> return ()


findFinishTransition :: Config -> DFAContext (Maybe (Int, String))
findFinishTransition config = do
  let endpoints = List.filter (\(_, _, r) -> List.null r) config
  case endpoints of
    [(e, l, [])] -> do 
      rules <- getRules
      --yield config
      let eRules = (\(a, _) -> a) <$> (rules Map.! e)
      let [exactRule] = List.filter ((==) (reverse l) . fst) (zip eRules ([0..] :: [Int]))
      (return . Just) (snd exactRule, e)
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
buildRuleMap = Map.fromList . (fmap (\(RuleDefinition s acts) -> (s, (\(RuleAction a code) -> (a, code)) <$> acts)))


generateFunctions :: TransitionMap -> ConfigurationMap -> RuleMap -> [String]
generateFunctions transitions configs rules = 
  let possibleWays = (\((a, b), c) -> (a, b, c)) <$> Map.toList transitions
      splitted = groupBy (\(a, _, _) (b, _, _) -> a == b) possibleWays
      stateNumber = length $ keys configs 
  in
  isSuccessDefinition :
  slrStateStackPeekDefinition : 
  restoreTokenStackDefinition :
  slrDispatcherDefinition stateNumber : 
  slrTokenStackPopDefinition : 
  slrTokenStackPushDefinition : 
  slrStateStackPushDefinition :
  slrStateStackPopDefinition : 
  (fmap (renderFunctionDef rules configs) splitted)


tripleFst :: (a, b, c) -> a
tripleFst (x, y, z) = x

isReduce :: TransitionType -> Bool 
isReduce t = case t of
  Reduce _ -> True
  _ -> False

renderFunctionDef :: RuleMap -> ConfigurationMap -> [(Int, TransitionType, Int)] -> String
renderFunctionDef rules configs list =
  let (nameInd, _, _) = head list
      functionName = "slrUnit_" ++ show nameInd 
      mainRuleName = (tripleFst $ head $ configs Map.! nameInd)
      exactRules = (\(a,b) -> a) <$> rules Map.! mainRuleName
      listedNonterminals = fst <$> (Map.toList rules)
      functionType = functionName ++ " :: SLRState _"
      functionHeader = functionName ++ " = do"
      bodyPrefix = "  pair <- slrTokenStackPop"
      debugPrefix = if isDebugMode then ("  yield " ++ show nameInd ++ " ") else ""
      caseHeader = "  case pair of"
      caseStatements = (\(ruleInd, t, i) -> generateCaseStatement listedNonterminals rules ruleInd exactRules t i) <$> list
  in intercalate "\n" ([functionHeader, bodyPrefix, debugPrefix, caseHeader] ++ caseStatements)


generateCaseStatement :: [String] -> RuleMap -> Int -> [[String]] -> TransitionType -> Int -> String
generateCaseStatement nontermList ruleMap ruleIndex rules ttype target = case ttype of
  Shift s -> renderShiftTransition ruleMap s target
  Reduce t -> if ruleIndex == 1 
                then "    Nothing -> return (\\[] -> Success, 0)" 
                else let fullRule = (ruleMap Map.! t) !! target in
                    renderReduceTransition nontermList t fullRule

findIndexList :: Eq a => [a] -> a -> Maybe Int
findIndexList list elem = snd <$> (safeHead $ List.filter (\(a, _) -> a == elem) $ zip list ([1..] :: [Int]))


renderShiftTransition :: RuleMap -> String -> Int -> String
renderShiftTransition rules s target = let index = if s `Map.member` rules then -1 else 0 in
  "    Just (" ++ show s ++ ", val) -> do slrStateStackPush " ++ show target ++ "; return (\\[] -> TokenValue val, " ++ show index ++ ")" 


renderReduceTransition :: [String] -> String -> ([String], [NameOrRef]) -> String
renderReduceTransition nonterminals nonterminalName (grammar, code) = 
  let len = List.length $ grammar 
      arguments = "[" ++ (intercalate ", " $ (\i -> "t" ++ show (len - 1 - i)) <$> [0..len-1]) ++ "]"
      mainIndex = fromJust $ findIndexList nonterminals nonterminalName in
  "    _ -> do \n" ++ 
  "      restoreTokenStack pair\n" ++
  generateStackReduce len ++ 
  "\n      slrTokenStackPush (" ++ show nonterminalName ++ ", \"UNDEFINED\")" ++
  "\n      return (\\" ++ arguments ++ " -> NontermValue" ++ show mainIndex ++ " $ " ++ createCode code grammar nonterminals ++ ", " ++ show len ++ ")"  


createCode :: [NameOrRef] -> [String] -> [String] -> String
createCode [] _ _ = ""
createCode (s : tail) grammar nonterminals = case s of
  Name a -> a ++ (' ' : createCode tail grammar nonterminals )
  Ref  i -> "(" ++ renderGetter i grammar nonterminals ++" t" ++ show i ++ ")" ++ (' ' : createCode tail grammar nonterminals) 

renderGetter :: Int -> [String] -> [String] -> String
renderGetter ind grammar nonterminals = 
  let exactRule = grammar !! ind
      getterId = findIndexList nonterminals exactRule in
  case getterId of
    Just i -> "getNValue" ++ show i
    Nothing -> "getTokenValue"

generateStackReduce :: Int -> String
generateStackReduce listSize = intercalate "\n" $ const "      slrStateStackPop"<$> [1..listSize]


slrStateStackPopDefinition :: String
slrStateStackPopDefinition = unlines [
   "slrStateStackPop = do"
  ,"  hd <- head <$> fst <$> get"
  ,"  modify $ bimap tail id"
  ,"  return hd" ] 


slrTokenStackPushDefinition :: String
slrTokenStackPushDefinition = unlines [
  "slrTokenStackPush tokenPair = do"
 ,"  modify $ bimap id ((:) tokenPair)"]


slrTokenStackPopDefinition :: String
slrTokenStackPopDefinition = unlines [
  "slrTokenStackPop = do"
 ,"  stack <- snd <$> get"
 ,"  if null stack then do"
 ,"    return Nothing"
 ,"  else do"
 ,"    let hd = head stack"
 ,"    modify $ bimap id tail"
 ,"    return $ Just hd"]


slrStateStackPushDefinition :: String 
slrStateStackPushDefinition = unlines [ 
  "slrStateStackPush ind = do"
 ,"  modify $ bimap ((:) ind) id"]


slrStateStackPeekDefinition :: String
slrStateStackPeekDefinition = "slrStateStackPeek :: SLRState Int\nslrStateStackPeek = head <$> fst <$> get"

slrDispatcherDefinition :: Int -> String
slrDispatcherDefinition maxStates = unlines $ 
  [ "slrDispatcher valueStack = do" ,
    "  lastState <- slrStateStackPeek",
    "  (producer, numToPop) <- case lastState of"] 
  ++ ((\i -> "    " ++ show i ++ " -> slrUnit_" ++ show i) <$> [0..maxStates-1])
  ++ ["  let (top, tail) = splitAt numToPop valueStack",
      "  if numToPop == -1 then slrDispatcher valueStack else do",
      "    let newValue = (producer top)",
      "    if isSuccess newValue then return valueStack else slrDispatcher (newValue : tail)"]


restoreTokenStackDefinition :: String
restoreTokenStackDefinition = unlines $
  ["restoreTokenStack s = case s of",
   "  Nothing -> return ()",
   "  Just a -> slrTokenStackPush a"]

isSuccessDefinition :: String
isSuccessDefinition = unlines $
  ["isSuccess e = case e of",
   "  Success -> True",
   "  _ -> False"] 
