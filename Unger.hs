{-# language ScopedTypeVariables #-}

module UngerNaiveEpsilon where

import Control.Monad(guard, join)
import Debug.Trace
import Data.Functor(($>))
import Data.List(inits, tails)
import Data.Map(Map)
import Data.Maybe(catMaybes, fromJust)
import qualified Data.Map as Map

import Common

-- create all possible partitions into `n` groups for a list.
-- you can avoid treating epsilon rules
-- by using `tail (inits xs)` and `init (tails xs)` instead
splits :: Int -> [a] -> [[[a]]]
splits 1 xs = [[xs]]
splits n xs = do
        (l, r) <- zip (inits xs) (tails xs)
        rec <- splits (n - 1) r
        return ((l:) rec)

unger :: forall n t. (Show n, Show t) => Eq t => Ord n =>
        CFG n t -> [t] -> Maybe (PForest n t)
unger cfg sent = ungerOr cfg sent (cfgStart cfg)

ungerOr :: forall n t. (Show n, Show t) => Eq t => Ord n =>
        CFG n t -> [t] -> n -> Maybe (PForest n t)
ungerOr cfg sent guess = let
        rhs = ruleCFG cfg guess
        mkLabelledPartitions n fs = fmap ((,) n . zip fs) $ splits (length fs) sent
        labelledPartitions = ([0..length rhs] `zip` rhs) >>= uncurry mkLabelledPartitions
        rec = (catMaybes (uncurry (ungerAnd cfg) <$> labelledPartitions))
        in guard (not $ null rec) $> POr guess rec

ungerAnd :: forall n t. (Show n, Show t) => Eq t => Ord n =>
        CFG n t -> Int -> [(Either n t, [t])] -> Maybe (PAnd n t)
ungerAnd cfg k ks = PAnd k <$> traverse rec ks
        where
        rec :: (Either n t, [t]) -> Maybe (PForest n t)
        rec (T t, ts) | ts == [t] = Just (PForestLeaf t)
                      | otherwise = Nothing
        rec (N n, ts)             = ungerOr cfg ts n

-- Unger's algorithm for parsing context-free grammars,
-- with epsilon-rules, without loops or any heuristics.
-- just a recognizer for now :/
ungerRec :: forall n t. (Show n, Show t) => Eq t => Ord n =>
        [t] -> CFG n t -> Bool
ungerRec ts cfg = go ts (cfgStart cfg) where
        go :: [t] -> n -> Bool
        go sent guess = let
                rhs :: [[Either n t]]
                rhs = ruleCFG cfg guess
                -- given one alternative for the current nonterminal,
                -- create all partitions of the current string labelled with
                -- each alternative's parts
                mkLabelledPartitions :: [Either n t] -> [[(Either n t, [t])]]
                mkLabelledPartitions fs = fmap (zip fs) $ splits (length fs) sent
                labelledPartitions :: [[(Either n t, [t])]]
                labelledPartitions = rhs >>= mkLabelledPartitions
                -- attempt to match terminals to terminals or recursively solve nonterminals
                validSubPartition :: (Either n t, [t]) -> Bool
                validSubPartition (T t,ts) = ts == [t]
                validSubPartition (N n,ts) = go ts n
                in any (all validSubPartition) labelledPartitions
