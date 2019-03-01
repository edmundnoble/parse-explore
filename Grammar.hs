{-# language ScopedTypeVariables #-}

module Grammar where

import Data.Either(isRight)
import Data.Maybe(catMaybes, listToMaybe)
import Data.Map.Strict(Map)
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace

import Common
import Types

-- this should be a closure algorithm too!
removeUndefined :: Ord n => CFG n t -> CFG n t
removeUndefined (CFG start rules) = let
        rulesWithRHS = Map.filter (not . null) rules
        allDefinedNonterminals = Map.keysSet rulesWithRHS
        defined (N n) = Set.member n allDefinedNonterminals
        defined (T _) = True
        newRules = filter (all defined) <$> rules
        in CFG start newRules

bindSet :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
bindSet f ss = Set.unions (f <$> Set.toList ss)

removeUnreachable :: forall n t. Ord n => CFG n t -> CFG n t
removeUnreachable cfg@(CFG start rules) = let
        startReachable = Set.singleton start
        reachableNonTerms = go startReachable
        reachable (N n) = Set.member n reachableNonTerms
        reachable (T _) = True
        newRules = filter (all reachable) <$> rules
        in CFG start newRules
        where
        reachableFromRules :: [[Either n t]] -> Set n
        reachableFromRules = Set.unions . fmap (Set.fromList . catMaybes . fmap matchNonterm)
        matchNonterm :: Either n t -> Maybe n
        matchNonterm (T t) = Nothing
        matchNonterm (N n) = Just n
        go reachableSoFar = let
                newlyReachable = (reachableFromRules . ruleCFG cfg) `bindSet` reachableSoFar
                in if newlyReachable == Set.empty
                then Set.empty
                else Set.union reachableSoFar (go newlyReachable)

removeNonProductive :: (Ord n, Show n) => CFG n t -> CFG n t
removeNonProductive cfg@(CFG start rules) = let
        begin = Map.keysSet $ Map.filter (any (all isRight)) rules
        continue sofar = let
                ruleIsProductive = all (either (`Set.member` sofar) (const True))
                in Map.keysSet $ Map.filter (any ruleIsProductive) rules
        allProductive = transitiveClosure begin continue
        removeNonProductiveNonTerminals rs = Map.filterWithKey (\k _ -> k `Set.member` allProductive) rules
        -- guaranteed never to return []
        -- also removes undefined nonterminals, because undefined nonterminals
        -- are not productive!
        removeNonProductiveRules = filter (
                all (either (`Set.member` allProductive) (const True)))
        newRules = fmap removeNonProductiveRules . removeNonProductiveNonTerminals $ rules
        in if null allProductive
                -- nothing we can do; all rules are non-productive
                then cfg
                else traceShow allProductive $ CFG start newRules
        where

transitiveClosure :: Ord fact => Set fact -> (Set fact -> Set fact) -> Set fact
transitiveClosure start continue = start `Set.union` go start
        where
        go sofar = let
                new = continue sofar
                in if new `Set.isSubsetOf` sofar
                then sofar
                else sofar `Set.union` go new

leftCornerSet :: (Ord n, Ord t) => CFG n t -> n -> Set (Either n t)
leftCornerSet cfg begin = transitiveClosure (Set.singleton (Left begin)) (bindSet explore)
        where
        explore (Right t) = Set.empty
        explore (Left n) = Set.fromList $ catMaybes $ listToMaybe <$> ruleCFG cfg n

data Test = S | A | B | C | D | E | F
        deriving (Eq, Ord, Show)

testGrammar = CFG S $ Map.fromList [
        (S, fmap N <$> [[A, B], [D, E]])
       ,(A, [[T 'a']])
       ,(B, [[T 'b', N C]])
       ,(C, [[T 'c']])
       ,(D, [[T 'd', N F]])
       ,(E, [[T 'e']])
       ,(F, [[T 'f', N D]])
        ]
