module DFAUtils where

import DFATypes
import Data.Map.Strict as Map
import Data.Set as Set
import Control.Monad.State.Strict
import Data.List as List
import Data.Maybe
import Data.Bifunctor

generateNewInitialState :: String
generateNewInitialState = "S'"


buildFollowMap :: RuleMap -> FollowMap
buildFollowMap ruleMap =
  let firstMap = fst $ execState (buildFirstMap ruleMap) (Map.empty, Set.empty)
 in
 execState (buildFollowMapLong ruleMap firstMap) Map.empty

buildFollowMapLong :: RuleMap -> FirstMap -> State FollowMap ()
buildFollowMapLong ruleMap firstMap =
  let incoming = (\(name, rules) -> (name, (\(grammar, _) -> grammar) <$> rules)) <$> (Map.toList ruleMap) in do
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
  map <- get
  let defaultSet = fromMaybe Set.empty $ map Map.!? target
  let succFirst = Set.fromList $ Just <$> (Set.toList $ safeMapGet firstMap succ)
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


