{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}

module Types where

import Control.Lens
import Control.Monad(join)

import Data.Function(on)
import Data.Tuple(swap)

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

import Data.Set(Set)
import qualified Data.Set as Set

import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

type CFRules n t = Map n [[Either n t]]

data CFG n t = CFG n (CFRules n t)
        deriving (Show)

type AnnotCFRules a n t = Map n (a, [[Either n t]])

data AnnotCFG n t a = ACFG n (AnnotCFRules a n t)
        deriving (Functor, Show)

data PTree n t = PNode !n ![PTree n t] | PLeaf !t
        deriving (Show)

data PAnd n t = PAnd !Int ![PForest n t] | PAndLeaf !t
        deriving (Show)

data PForest n t = POr !n ![PAnd n t] | PForestLeaf !t
        deriving (Show)

{-# complete T, N #-}

pattern T t = Right t
pattern N n = Left n

lit fa = fmap Right fa

-- Chomsky normal form CFG: all nonterminals rewrite to either a single terminal
-- or two nonterminals.
-- there's also a map from terminals to the nonterminals that derive them for driving
-- the first recognition phase of CYK parsing
data CNFCFG n t = CNFCFG n (Map n (NonEmpty (Either (n,n) t))) (Map (Either (n,n) t) (NonEmpty n))
        deriving (Eq, Ord, Show)

cnfCFGToCFG :: (Ord n, Ord t) => CNFCFG n t -> CFG n t
cnfCFGToCFG (CNFCFG n rules _) = CFG n $ (fmap . fmap) convertRHS $ NonEmpty.toList <$> rules
        where
        convertRHS (Left (n1,n2)) = [Left n1,Left n2]
        convertRHS (Right t) = [Right t]

groupEm :: forall a b. Ord a => [(a, b)] -> Map a (NonEmpty b)
groupEm = Map.fromListWith (<>) . over (mapped._2) pure

-- blatantly stolen from Distribution.Utils.Generic
ordNubBy :: Ord b => (a -> b) -> [a] -> [a]
ordNubBy f l = go Set.empty l
  where
    go !_ [] = []
    go !s (x:xs)
      | y `Set.member` s = go s xs
      | otherwise        = let !s' = Set.insert y s
                            in x : go s' xs
      where
        y = f x

mkCNFCFG :: (Ord n, Ord t) => n -> Map n (NonEmpty (Either (n,n) t)) -> CNFCFG n t
mkCNFCFG n rules = CNFCFG n rules backwardRules
        where
        getRules = join . traverse (NonEmpty.toList . sequence) . Map.toList
        getReversedRules = fmap swap . getRules
        backwardRules = groupEm . ordNubBy id $ getReversedRules $ rules

-- no epsilon rules
data CFNonEmptyCFG n t = CFNonEmptyCFG n (CFNonEmptyRules n t)

type CFNonEmptyRules n t = Map n (NonEmpty (Either n t))
