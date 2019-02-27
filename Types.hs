{-# language DeriveFunctor #-}
{-# language PatternSynonyms #-}

module Types where

import Data.Map.Strict(Map)
import Data.List.NonEmpty(NonEmpty)
import qualified Data.Map.Strict as Map

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
data CNFCFG n t = CNFCFG n (CNFRules n t)

type CNFRules n t = Map n (Either t (n, n))

data CFNonEmptyCFG n t = CFNonEmptyCFG n (CFNonEmptyRules n t)

type CFNonEmptyRules n t = Map n (NonEmpty (Either n t))
