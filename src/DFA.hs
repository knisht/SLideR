module DFA where

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}


import TemplateGrammar
import Data.Set as Set
import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Maybe
import Text.Regex
import Data.Bifunctor
import Data.List as List
import Data.Maybe as Maybe
import Debug

type IndexedState = (String, [String], [String])
type RuleMap = Map String [([String], [Assign])]
type Config = [IndexedState]
type Context a = State RuleMap a
type ConfigurationMap = Map Int Config 
type Transition = (Int, TransitionType, Int)
data TransitionType = Shift String | Reduce String deriving (Show, Eq, Ord)
type TransitionMap = Map (Int, TransitionType) Int
type DFAContext a = State (TransitionMap, ConfigurationMap, RuleMap) a
type FollowMap = Map String (Set (Maybe String))
type FirstMap = Map String (Set String)
type Attributes = [String]


infixr 9 ∘
(∘) = (.)


isDebugMode = False 

buildDFA :: [String] -> [RuleDefinition] -> String
buildDFA attributes ruleDefs = 
  let (RuleDefinition oldInitial _) = head ruleDefs
      dummyAssign = (\s -> Assign s ("$0." ++ s)) <$> attributes
      initial = RuleDefinition generateNewInitialState [RuleAction [oldInitial] (dummyAssign)]
      newList = initial : ruleDefs
      nonterminals = collectNonterminals (initial : ruleDefs)
      ruleMap = {-Map.insert generateNewInitialState [[oldInitial]] $-} buildRuleMap newList
      typeContainer = generateTypeContainer (length newList)
      followMap = buildFollowMap ruleMap
      typeGetters = generateTypeGetters (length newList)
      attrGetters = generateAttrGetters attributes
      (transits, configs, rules) = execState (buildCompleteTransitionTable initial) (Map.empty, Map.empty, ruleMap)
      functions = generateFunctions attributes followMap transits configs rules 
      mainEval = mainEvaluator oldInitial ruleMap
  in intercalate "\n\n\n" (mainEval : typeContainer : typeGetters : attrGetters : functions)


mainEvaluator :: String -> RuleMap -> String
mainEvaluator target rules = 
  let list = fst <$> Map.toList rules
      getterIndex = fromJust $ findIndexList list target in 
  "mainEvaluator input = getNValue" ++ show getterIndex ++ " $ head $ evalState (slrDispatcher []) ([0], slrLexer input)"

generateTypeContainer :: Int -> String
generateTypeContainer number = intercalate "\n" $ 
  ("data ValueContainer " ++ 
  (intercalate " " $ (\i -> "t" ++ show i) <$> [1..number]) ++ " = Dummy") : 
  ("  | Success" : 
   "  | TokenValue String" : 
   ((\i -> "  | NontermValue" ++ show i ++ " t" ++ show i) <$> [1..number]) ++ ["  deriving Show"])


generateAttrGetters :: Attributes -> String
generateAttrGetters attrs = intercalate "\n\n" $ generateAttrGetter (length attrs) <$> zip attrs [0..]

generateAttrGetter :: Int -> (String, Int) -> String
generateAttrGetter maxargs (name, ind) = "getAttr_" ++ name ++ " (" ++ intercalate ", " (renderArgs maxargs ind) ++ ") = t"
  
renderArgs :: Int -> Int -> [String]
renderArgs 0 _ = []
renderArgs i 0 = "t" : renderArgs (i-1) (-1)
renderArgs i j = "_" : renderArgs (i-1) (j-1)

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


buildFollowMap :: RuleMap -> FollowMap
buildFollowMap ruleMap = 
  let firstMap = fst $ execState (buildFirstMap ruleMap) (Map.empty, Set.empty)
 in
 execState (buildFollowMapLong ruleMap firstMap) Map.empty

buildFollowMapLong :: RuleMap -> FirstMap -> State FollowMap ()
buildFollowMapLong ruleMap firstMap = 
  let incoming = (\(name, rules) -> (name, (\(grammar, _) -> grammar) <$> rules)) <$> (Map.toList ruleMap) in do
  --yield firstMap
  let traverseF f = (\(name, list) -> traverse (f ruleMap name firstMap) list)
  traverse (traverseF buildFollowMapInternal) incoming 
  repeatedFollowEnrich incoming firstMap ruleMap
  return ()


repeatedFollowEnrich :: [(String, [[String]])] -> FirstMap -> RuleMap -> State FollowMap ()
repeatedFollowEnrich incoming firstMap ruleMap = do
  currentMap <- get
  let traverseF f = (\(name, list) -> traverse (f ruleMap name firstMap) list)
  traverse (traverseF buildFollowMapInternalAfter) incoming
  newMap <- get 
  if currentMap == newMap then return () else repeatedFollowEnrich incoming firstMap ruleMap
  
  

buildFollowMapInternal :: RuleMap -> String -> FirstMap -> [String] -> State FollowMap ()
buildFollowMapInternal ruleMap node firsts rules = do
  --yield $ node ++ " " ++ show rules
  if node == generateNewInitialState then do
    folmap <- get
    let existing = fromMaybe Set.empty (folmap Map.!? node)
    modify (Map.insert node (existing `Set.union` (Set.singleton Nothing)))
  else
    return ()
  let newList = List.filter (flip Map.member ruleMap . fst) $ zip rules $ tail (rules)
  traverse (uniteFollowSets firsts) newList
  return ()
  

buildFollowMapInternalAfter :: RuleMap -> String -> FirstMap -> [String] -> State FollowMap ()
buildFollowMapInternalAfter ruleMap node firsts rules = do
  let lastRule = last rules
  if Map.member lastRule ruleMap then do
    map <- get
    let defaultSet = safeMapGet map lastRule
    let current = safeMapGet map node
    modify (Map.insert lastRule (defaultSet `Set.union` current))
  else return ()
  return ()
  

uniteFollowSets :: FirstMap -> (String, String) -> State FollowMap ()
uniteFollowSets firstMap (target, succ) = do
  --yield (target, succ)
  map <- get
  let defaultSet = fromMaybe Set.empty $ map Map.!? target
  let succFirst = Set.fromList $ Just <$> (Set.toList $ safeMapGet firstMap succ)
  --yield succFirst
  modify (Map.insert target (defaultSet `Set.union` succFirst))


buildFirstMap :: RuleMap -> State (FirstMap, Set String) ()
buildFirstMap ruleMap = do
  let nodes = nub $ join . join $ (\(_, list) -> (\(exactRules, _) -> exactRules) <$> list) <$> Map.toList ruleMap  
  traverse (buildFirstMapInternal ruleMap) nodes
  return ()


buildFirstMapInternal :: RuleMap -> String -> State (FirstMap, Set String) ()
buildFirstMapInternal ruleMap node = do
  visited <- snd <$> get
  if node `Set.member` visited then
    return ()
  else do
    modify $ bimap id (Set.insert node)  
    if (Map.member node ruleMap) then do
      let rules = (\(a, _) -> a) <$> (ruleMap Map.! node)
      lists <- traverse (buildFirstMapInternal ruleMap) (head <$> rules)
      traverse (uniteFirstSets node) (head <$> rules) 
      return () 
    else 
      modify $ bimap (Map.insert node (Set.singleton node)) id


uniteFirstSets :: String -> String -> State (FirstMap, Set String) ()
uniteFirstSets node s = do
  map <- fst <$> get
  let united = (safeMapGet map s) `Set.union` (safeMapGet map node)
  modify $ bimap (Map.insert node united) id


safeMapGet :: (Ord k, Monoid m) => Map k m -> k -> m
safeMapGet map k = fromMaybe mempty (map Map.!? k)

buildRuleMap :: [RuleDefinition] -> RuleMap
buildRuleMap = Map.fromList . (fmap (\(RuleDefinition s acts) -> (s, (\(RuleAction a code) -> (a, code)) <$> acts)))


generateFunctions :: Attributes -> FollowMap -> TransitionMap -> ConfigurationMap -> RuleMap -> [String]
generateFunctions attributes followMap transitions configs rules = 
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
  (fmap (renderFunctionDef attributes rules configs followMap) splitted)


tripleFst :: (a, b, c) -> a
tripleFst (x, y, z) = x

isReduce :: TransitionType -> Bool 
isReduce t = case t of
  Reduce _ -> True
  _ -> False

renderFunctionDef :: Attributes -> RuleMap -> ConfigurationMap -> FollowMap -> [(Int, TransitionType, Int)] -> String
renderFunctionDef attributes rules configs followMap list =
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
      helperName = "slrReduceHelper_" ++ show nameInd
      caseStatements = (\(ruleInd, t, i) -> generateCaseStatement attributes helperName followMap listedNonterminals rules ruleInd exactRules t i) <$> list
      helperDefinition = fromMaybe "" $ safeHead $ List.filter ((/=) "") $ snd <$> caseStatements
  in intercalate "\n" ([functionHeader, bodyPrefix, debugPrefix, caseHeader] ++ (fst <$> caseStatements)) ++ "\n\n" ++ helperDefinition 


generateCaseStatement :: Attributes -> String -> FollowMap -> [String] -> RuleMap -> Int -> [[String]] -> TransitionType -> Int -> (String, String)
generateCaseStatement attributes helperName follows nontermList ruleMap ruleIndex rules ttype target = case ttype of
  Shift s -> renderShiftTransition ruleMap s target
  Reduce t -> if ruleIndex == 1 
                then ("    Nothing -> return (\\[] -> Success, 0)", "") 
                else let fullRule = (ruleMap Map.! t) !! target in
                    renderReduceTransition attributes helperName follows nontermList t fullRule

findIndexList :: Eq a => [a] -> a -> Maybe Int
findIndexList list elem = snd <$> (safeHead $ List.filter (\(a, _) -> a == elem) $ zip list ([1..] :: [Int]))


renderShiftTransition :: RuleMap -> String -> Int -> (String, String)
renderShiftTransition rules s target = let index = if s `Map.member` rules then -1 else 0 in
  ("    Just (" ++ show s ++ ", val) -> do slrStateStackPush " ++ show target ++ "; return (\\[] -> TokenValue val, " ++ show index ++ ")", "")


renderReduceTransition :: Attributes -> String -> FollowMap -> [String] -> String -> ([String], [Assign]) -> (String, String)
renderReduceTransition attributes helperName follows nonterminals nonterminalName (grammar, code) = 
  let reducingHelper = renderReducingBody attributes helperName nonterminals nonterminalName (grammar, code) 
      followSet = Set.toList $ follows Map.! nonterminalName 
      gotos = intercalate "\n" $ (\s -> "    " ++ redraw s ++ " -> " ++ helperName ++ " pair") <$> followSet in
  (gotos, reducingHelper)


redraw :: Maybe String -> String
redraw e = case e of
  Just s -> "Just (" ++ show s ++ ", _)"
  Nothing -> "Nothing" 

renderReducingBody :: Attributes -> String -> [String] -> String -> ([String], [Assign]) -> String 
renderReducingBody attributes helperName nonterminals nonterminalName (grammar, code) = 
  let len = List.length $ grammar
      arguments = "[" ++ (intercalate ", " $ (\i -> "t" ++ show (len - 1 - i)) <$> [0..len-1]) ++ "]"
      mainIndex = fromJust $ findIndexList nonterminals nonterminalName in
  helperName ++ " :: Maybe (String, String) -> SLRState _\n" ++
  helperName ++ " pair = do \n" ++ 
  "  restoreTokenStack pair\n" ++
  generateStackReduce len ++ 
  "\n  slrTokenStackPush (" ++ show nonterminalName ++ ", \"UNDEFINED\")" ++
  "\n  return (\\" ++ arguments ++ " -> NontermValue" ++ show mainIndex ++ " (" ++ createCode attributes code grammar nonterminals ++ "), " ++ show len ++ ")"


createCode :: Attributes -> [Assign] -> [String] -> [String] -> String
createCode attributes assigns grammar nonterminals = 
  let codes = (\a -> createSingleCode attributes a grammar nonterminals) <$> assigns
      numbers = (\(Assign name _) -> findIndexList attributes name) <$> assigns
      zipped = zip codes numbers
      sorted = fst <$> sortOn snd zipped in
  "(" ++ intercalate ", " sorted ++ ")"


createSingleCode :: Attributes -> Assign -> [String] -> [String] -> String
createSingleCode attributes (Assign _ code) grammar nonterminals =
  let regexProducer i = mkRegex $ "\\$" ++ show i ++ "\\.(\\w+)?\\b"
      substProducer i = "(" ++ renderGetter i grammar nonterminals ++ " t" ++ show i ++ ")"
      substs = join $ (\i -> generateSubstitutors i attributes grammar nonterminals) <$> [0..length grammar -1] 
      trueCode = tail $ List.take (length code - 1) code in
  List.foldr (\(regex, subst) str -> subRegex regex str subst) trueCode substs


generateSubstitutors :: Int -> Attributes -> [String] -> [String] -> [(Regex, String)]
generateSubstitutors ind attributes grammar nonterminals =
  let typeGetter = "(" ++ renderGetter ind grammar nonterminals ++ " t" ++ show ind ++ ")"
      valueGetter s = "(getAttr_" ++ s ++ " " ++ typeGetter ++ ")" 
      regexBase = "\\$" ++ show ind in
  (mkRegex $ "\\$" ++ show ind, typeGetter) : 
  ((\s -> (mkRegex $ regexBase ++ "\\." ++ s, valueGetter s)) <$> attributes)       


renderGetter :: Int -> [String] -> [String] -> String
renderGetter ind grammar nonterminals = 
  let exactRule = grammar !! ind
      getterId = findIndexList nonterminals exactRule in
  case getterId of
    Just i -> "getNValue" ++ show i
    Nothing -> "getTokenValue"


generateStackReduce :: Int -> String
generateStackReduce listSize = intercalate "\n" $ const "  slrStateStackPop"<$> [1..listSize]


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
 ,"  if Prelude.null stack then do"
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
  ++ ["  let (top, tail) = Prelude.splitAt numToPop valueStack",
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
