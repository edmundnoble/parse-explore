{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}

module Heur where

import Control.Monad.State
import Control.Monad.State.Class(MonadState(..))
import Data.Bits((.|.))
import Data.Hashable
import Data.Map(Map)
import Data.Set(Set)
import Data.Semigroup(Max(..), Min(..), Sum(..))
import Data.List(sortOn)
import Data.Maybe(fromJust, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Numeric

import Common
import Types

trivialAnnot :: CFG n t -> AnnotCFG n t ()
trivialAnnot (CFG start rules) = ACFG start (fmap ((),) rules)

annotCFG :: CFG n t -> (n -> [[Either n t]] -> a) -> AnnotCFG n t a
annotCFG (CFG start rules) annot = ACFG start (annotRules rules)
        where annotRules = Map.mapWithKey (\k rhs -> (annot k rhs, rhs))

acfgStart :: AnnotCFG n t a -> n
acfgStart (ACFG s _) = s

newtype Bloom = Bloom Int
        deriving (Eq)

instance Show Bloom where
        showsPrec _ (Bloom bl) = showIntAtBase 2 (\case 0 -> '0'; 1 -> '1') bl

instance Semigroup Bloom where
        (Bloom i) <> (Bloom j) = Bloom (i .|. j)

instance Monoid Bloom where
        mempty = Bloom 0

data CompoundAnnot = CompoundAnnot !Bloom !FancyMin !FancyMax
        deriving Show

instance Semigroup CompoundAnnot where
        (CompoundAnnot bl mi ma) <> (CompoundAnnot bl' mi' ma') =
                CompoundAnnot (bl <> bl') (mi <> mi') (ma <> ma')

type CCFG n t = AnnotCFG n t CompoundAnnot

instance Monoid CompoundAnnot where
        mempty = CompoundAnnot mempty mempty mempty

newtype FancyMin = FancyMin (Maybe (Min Int)) deriving (Monoid, Semigroup) via (Maybe (Min Int))
        deriving Show

newtype FancyMax = FancyMax (Maybe (Max Int))
        deriving Show

instance Monoid FancyMax where
        mempty = FancyMax (Just 0)

instance Semigroup FancyMax where
        (FancyMax ma) <> (FancyMax ma') =
                FancyMax $ (<>) <$> ma <*> ma'

newtype FancySum = FancySum (Maybe Int)

instance Monoid FancySum where
        mempty = FancySum (Just 0)

instance Semigroup FancySum where
        (FancySum su) <> (FancySum su') =
                FancySum $ (+) <$> su <*> su'

foldAnnot :: forall n t a. Ord n =>
        (t -> a) ->
        a ->
        ([a] -> a) ->
        ([a] -> a) ->
        CFG n t ->
        AnnotCFG n t a
foldAnnot measure empty sq pr cfg@(CFG start rules) =
        ACFG start (Map.intersectionWith (,) allMeasures rules)
        where
        allMeasures = execState (traverse goSt allNonTerms) Map.empty
        goSt sym = modify (\ms -> Map.insert sym (go sym Set.empty ms) ms)
        allNonTerms = fst <$> Map.toList rules
        go :: n -> Set n -> Map n a -> a
        go sym seen measures = let
                rhs = ruleCFG cfg sym
                measRec (Left n) =
                        fromMaybe (if Set.member n seen then empty else go n (Set.insert n seen) measures) (Map.lookup n measures)
                measRec (Right t) = measure t
                measRHS = (fmap . fmap) measRec rhs
                totalMeas = sq (pr <$> measRHS)
                in totalMeas

blum :: Hashable t => t -> Bloom
blum t = Bloom (hashWithSalt 100 t)

checkblum :: Hashable t => [t] -> Bloom -> Bool
checkblum ts bl = let bl' = foldMap blum ts in bl' <> bl == bl

compoundAnnot :: Ord n => Hashable t => CFG n t -> AnnotCFG n t CompoundAnnot
compoundAnnot = foldAnnot (\t -> CompoundAnnot (blum t) (FancyMin (Just 1)) (FancyMax (Just 1))) (CompoundAnnot mempty mempty (FancyMax Nothing)) sq pr
        where
        pr as = case foldMap (\(CompoundAnnot bl (FancyMin mi) (FancyMax ma)) -> (bl, FancySum (getMin <$> mi), FancySum (getMax <$> ma))) as of
                (bl, FancySum mi, FancySum ma) -> CompoundAnnot bl (FancyMin (Min <$> mi)) (FancyMax (Max <$> ma))
        sq as = foldMap id as

matchesCompoundAnnot :: Hashable t => CompoundAnnot -> [t] -> Bool
matchesCompoundAnnot (CompoundAnnot bl (FancyMin (fmap getMin -> mi)) (FancyMax (fmap getMax -> ma))) ts =
        let len = length ts in
                maybe True (<= len) mi &&
                maybe True (>= len) ma &&
                checkblum ts bl
