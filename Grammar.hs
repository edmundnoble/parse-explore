module Grammar where

import Data.Map.Strict(Map)
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Common
import Types

removeUndefined :: Ord n => CFG n t -> CFG n t
removeUndefined (CFG start rs) = let
        allDefinedNonterminals = Set.singleton start `Set.union` (Map.keysSet rs)
        defined (N n) = Set.member n allDefinedNonterminals
        defined (T _) = True
        newRules = filter (all defined) <$> rs
        in CFG start newRules
