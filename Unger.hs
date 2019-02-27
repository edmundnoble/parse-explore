{-# language ScopedTypeVariables #-}

module Unger where

import Control.Monad(guard, join)
import Control.Monad.State
import Debug.Trace
import Data.ByteString(ByteString)
import Data.Char(ord)
import Data.Functor(($>))
import Data.Functor.Compose
import Data.Hashable
import Data.List(inits, tails)
import Data.Map(Map)
import Data.Maybe(catMaybes, fromJust)
import Data.Trie(Trie)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Trie as Trie

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

-- unlike the above version, `BS.inits` is linear instead of quadratic.
splitsBS :: Int -> ByteString -> [[ByteString]]
splitsBS 1 xs = [[xs]]
splitsBS n xs = do
        (l, r) <- zip (BS.inits xs) (BS.tails xs)
        rec <- splitsBS (n - 1) r
        rec `seq` return (l:rec)

ungerBS :: forall n. (Show n) => Ord n =>
        CFG n Char -> ByteString -> Maybe (PForest n Char)
ungerBS cfg sent = ungerOrBS cfg sent (cfgStart cfg)

ungerOrBS :: forall n. (Show n) => Ord n =>
        CFG n Char -> ByteString -> n -> Maybe (PForest n Char)
ungerOrBS cfg sent guess = let
        rhs = ruleCFG cfg guess
        mkLabelledPartitions n fs = fmap ((,) n . zip fs) $ splitsBS (length fs) sent
        labelledPartitions = ([0..length rhs] `zip` rhs) >>= uncurry mkLabelledPartitions
        rec = (catMaybes (uncurry (ungerAndBS cfg) <$> labelledPartitions))
        in guard (not $ null rec) $> POr guess rec

ungerAndBS :: forall n. (Show n) => Ord n =>
        CFG n Char -> Int -> [(Either n Char, ByteString)] -> Maybe (PAnd n Char)
ungerAndBS cfg k ks = PAnd k <$> ((guard (all matching ks)) *> traverse rec ks)
        where
        -- an extra "breadth-firsty" stage to get us terminating
        matching :: (Either n Char, ByteString) -> Bool
        matching (T t, ts) = BS.length ts == 1 && BSU.unsafeHead ts == fromIntegral (ord t)
        matching _         = True
        rec :: (Either n Char, ByteString) -> Maybe (PForest n Char)
        rec (T t, _)  = Just (PForestLeaf t)
        rec (N n, ts) = ungerOrBS cfg ts n

heurUnger :: forall n t. Hashable t => (Show n, Show t) => Eq t => Ord n =>
        CCFG n t -> [t] -> Maybe (PForest n t)
heurUnger acfg sent = heurUngerOr acfg sent (acfgStart acfg)

heurUngerOr :: forall n t. Hashable t => (Show n, Show t) => Eq t => Ord n =>
        CCFG n t -> [t] -> n -> Maybe (PForest n t)
heurUngerOr acfg sent guess = let
        (_, rhs) = ruleACFG acfg guess
        mkLabelledPartitions n fs = fmap ((,) n . zip fs) $ splits (length fs) sent
        labelledPartitions = ([0..length rhs] `zip` rhs) >>= uncurry mkLabelledPartitions
        rec = (catMaybes (uncurry (heurUngerAnd acfg) <$> labelledPartitions))
        in guard (not $ null rec) $> POr guess rec

heurUngerAnd :: forall n t. Hashable t => (Show n, Show t) => Eq t => Ord n =>
        CCFG n t -> Int -> [(Either n t, [t])] -> Maybe (PAnd n t)
heurUngerAnd acfg@(ACFG _ rules) k ks = PAnd k <$> ((guard (all matching ks)) *> traverse rec ks)
        where
        -- an extra "breadth-firsty" stage to get us terminating
        matching :: (Either n t, [t]) -> Bool
        matching  (T t, ts) = ts == [t]
        matching  (N n, ts) = matchesCompoundAnnot (fromJust (fst <$> Map.lookup n rules)) ts
        rec :: (Either n t, [t]) -> Maybe (PForest n t)
        rec (T t, ts) = Just (PForestLeaf t)
        rec (N n, ts) = heurUngerOr acfg ts n

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
ungerAnd cfg k ks = PAnd k <$> ((guard (all matching ks)) *> traverse rec ks)
        where
        -- an extra "breadth-firsty" stage to get us terminating
        matching :: (Either n t, [t]) -> Bool
        matching  (T t, ts) = ts == [t]
        matching  _ = True
        rec :: (Either n t, [t]) -> Maybe (PForest n t)
        rec (T t, ts) | ts == [t] = Just (PForestLeaf t)
                      | otherwise = Nothing
        rec (N n, ts)             = ungerOr cfg ts n

type Memo n t =
        State (Map (n, [t]) (Maybe (PForest n t)))

type MemoBS n t =
        State (Map (n, ByteString) (Maybe (PForest n t)))

type TrieMemoBS n t =
        State (Map n (Trie (Maybe (PForest n t))))

ungerMemo :: forall n t. (Show n, Show t) => Eq t => Ord n => Ord t =>
        CFG n t ->
        [t] ->
        Maybe (PForest n t)
ungerMemo cfg sent = flip evalState Map.empty $ ungerOrMemo cfg sent (cfgStart cfg)

ungerOrMemo :: forall n t. (Show n, Show t) => Eq t => Ord n => Ord t =>
        CFG n t ->
        [t] ->
        n ->
        Memo n t (Maybe (PForest n t))
ungerOrMemo cfg sent guess = let
        rhs = ruleCFG cfg guess
        mkLabelledPartitions n fs = fmap ((,) n . zip fs) $ splits (length fs) sent
        labelledPartitions = ([0..length rhs] `zip` rhs) >>= uncurry mkLabelledPartitions
        in do
                memo <- get
                case Map.lookup (guess,sent) memo of
                        Nothing -> do
                                rec <- (catMaybes <$> (traverse (uncurry (ungerAndMemo cfg)) labelledPartitions))
                                let out = (guard (not $ null rec) $> POr guess rec)
                                put (Map.insert (guess,sent) out memo)
                                return out
                        Just pfs -> return pfs

ungerAndMemo :: forall n t. (Show n, Show t) => Eq t => Ord n => Ord t =>
        CFG n t ->
        Int ->
        [(Either n t, [t])] ->
        Memo n t (Maybe (PAnd n t))
ungerAndMemo cfg k ks = do
        a <- getCompose $ traverse (Compose . rec) ks
        let _ = a :: Maybe [PForest n t]
        let match = ((guard (all matching ks)) *>)
        return (PAnd k <$> (match a))
        where
        -- an extra "breadth-firsty" stage to get us terminating
        matching :: (Either n t, [t]) -> Bool
        matching  (T t, ts) = ts == [t]
        matching  _ = True
        rec :: (Either n t, [t]) -> Memo n t (Maybe (PForest n t))
        rec (T t, ts) = pure (Just (PForestLeaf t))
        rec (N n, ts) = ungerOrMemo cfg ts n

ungerMemoBS :: forall n. (Show n) => Ord n =>
        CFG n Char ->
        ByteString ->
        Maybe (PForest n Char)
ungerMemoBS cfg sent = flip evalState Map.empty $ ungerOrMemoBS cfg sent (cfgStart cfg)

ungerOrMemoBS :: forall n. (Show n) => Ord n =>
        CFG n Char ->
        ByteString ->
        n ->
        MemoBS n Char (Maybe (PForest n Char))
ungerOrMemoBS cfg sent guess = let
        rhs = ruleCFG cfg guess
        mkLabelledPartitions n fs = fmap ((,) n . zip fs) $ splitsBS (length fs) sent
        labelledPartitions = ([0..length rhs] `zip` rhs) >>= uncurry mkLabelledPartitions
        in do
                memo <- get
                case Map.lookup (guess,sent) memo of
                        Nothing -> do
                                rec <- (catMaybes <$> (traverse (uncurry (ungerAndMemoBS cfg)) labelledPartitions))
                                let out = (guard (not $ null rec) $> POr guess rec)
                                put (Map.insert (guess,sent) out memo)
                                return out
                        Just pfs -> return pfs

ungerAndMemoBS :: forall n. (Show n) => Ord n =>
        CFG n Char ->
        Int ->
        [(Either n Char, ByteString)] ->
        MemoBS n Char (Maybe (PAnd n Char))
ungerAndMemoBS cfg k ks = do
        a <- getCompose $ traverse (Compose . rec) ks
        let match = ((guard (all matching ks)) *>)
        return (PAnd k <$> (match a))
        where
        -- an extra "breadth-firsty" stage to get us terminating
        matching :: (Either n Char, ByteString) -> Bool
        matching (T t, ts) = BS.length ts == 1 && BSU.unsafeHead ts == fromIntegral (ord t)
        matching  _ = True
        rec :: (Either n Char, ByteString) -> MemoBS n Char (Maybe (PForest n Char))
        rec (T t, ts) = pure (Just (PForestLeaf t))
        rec (N n, ts) = ungerOrMemoBS cfg ts n

ungerTrieMemoBS :: forall n. (Show n) => Ord n =>
        CFG n Char ->
        ByteString ->
        Maybe (PForest n Char)
ungerTrieMemoBS cfg sent = flip evalState Map.empty $ ungerOrTrieMemoBS cfg sent (cfgStart cfg)

ungerOrTrieMemoBS :: forall n. (Show n) => Ord n =>
        CFG n Char ->
        ByteString ->
        n ->
        TrieMemoBS n Char (Maybe (PForest n Char))
ungerOrTrieMemoBS cfg sent guess = let
        rhs = ruleCFG cfg guess
        mkLabelledPartitions n fs = fmap ((,) n . zip fs) $ splitsBS (length fs) sent
        labelledPartitions = ([0..length rhs] `zip` rhs) >>= uncurry mkLabelledPartitions
        in do
                memo <- get
                let recurse = catMaybes <$> (traverse (uncurry (ungerAndTrieMemoBS cfg)) labelledPartitions)
                let guardedRecurse = fmap (\r -> guard (not $ null r) $> POr guess r) recurse
                case Map.lookup guess memo of
                        Nothing -> do r <- guardedRecurse; put (Map.insert guess (Trie.singleton sent r) memo); return r
                        Just underGuess -> case Trie.lookup sent underGuess of
                                Just pfs -> return pfs
                                Nothing -> do r <- guardedRecurse; put (Map.insert guess (Trie.insert sent r underGuess) memo); return r

ungerAndTrieMemoBS :: forall n. (Show n) => Ord n =>
        CFG n Char ->
        Int ->
        [(Either n Char, ByteString)] ->
        TrieMemoBS n Char (Maybe (PAnd n Char))
ungerAndTrieMemoBS cfg k ks = do
        a <- getCompose $ traverse (Compose . rec) ks
        let match = ((guard (all matching ks)) *>)
        return (PAnd k <$> (match a))
        where
        -- an extra "breadth-firsty" stage to get us terminating
        matching :: (Either n Char, ByteString) -> Bool
        matching (T t, ts) = BS.length ts == 1 && BSU.unsafeHead ts == fromIntegral (ord t)
        matching  _ = True
        rec :: (Either n Char, ByteString) -> TrieMemoBS n Char (Maybe (PForest n Char))
        rec (T t, ts) = pure (Just (PForestLeaf t))
        rec (N n, ts) = ungerOrTrieMemoBS cfg ts n


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
