module DFATypes where

import TemplateGrammar
import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Set

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


