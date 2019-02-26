{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}

module Common where

import Data.Map(Map)
import Data.Maybe(fromJust)
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen

data CFG n t = CFG n (CFRules n t)
        deriving (Show)

type CFRules n t = Map n [[Either n t]]

data PTree n t = PNode n [PTree n t] | PLeaf t
        deriving (Show)

data PAnd n t = PAnd Int [PForest n t] | PAndLeaf t
        deriving (Show)

data PForest n t = POr n [PAnd n t] | PForestLeaf t
        deriving (Show)

startCFG :: CFG n t -> n
startCFG (CFG s _) = s

ruleCFG :: Ord n => CFG n t -> n -> [[Either n t]]
ruleCFG (CFG _ rs) n = fromJust (Map.lookup n rs)

{-# complete T, N #-}

pattern T t = Right t
pattern N n = Left n

lit fa = fmap Right fa
