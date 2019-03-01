{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}

module Common where

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

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Types

startCFG :: CFG n t -> n
startCFG (CFG s _) = s

ruleCFG :: Ord n => CFG n t -> n -> [[Either n t]]
ruleCFG (CFG _ rs) n = fromJust (Map.lookup n rs)

ruleACFG :: Ord n => AnnotCFG n t a -> n -> (a, [[Either n t]])
ruleACFG (ACFG _ rs) n = fromJust (Map.lookup n rs)

shortestRulesFirst :: CFG n t -> CFG n t
shortestRulesFirst (CFG start rules) =
        CFG start $ fmap (sortOn length) rules

lit fa = fmap Right fa

data TDH
        = Name
        | Sentence
        | List
        deriving (Eq, Ord, Show)

tdhGram = shortestRulesFirst $ CFG Sentence (Map.fromList [
        (Name, (fmap T <$> ["tom", "dick", "harry"])),
        (Sentence, [[N Name], [N List] ++ (T <$> "and") ++ [Left Name]]),
        (List, [[Left Name, Right ',', Left List], [Left Name]])
        ])

tdhSample1 = "tom"
tdhSample2 = "tom,dickandharry"
tdhSample3 = "tomanddick"

data Arith
        = Expr
        | Term
        | Factor
        deriving (Eq, Ord, Show)

arithGram = shortestRulesFirst $ CFG Expr (Map.fromList [
        (Expr, [[N Expr, T '+', N Term], [N Term]]),
        (Term, [[N Term, T '*', N Factor], [N Factor]]),
        (Factor, [[T '(', N Expr, T ')'], [T 'i']])
        ])

arithSample1 = "(i+i)*i"
arithSample2 = "i"
arithSample3 = "i+i"
arithSampleBeeg = go 3 where
        go 0 = "i"
        go n = "(" ++ go (n - 1) ++ "+" ++ go (n - 1) ++ ")"

arithAmbiGram = shortestRulesFirst $ CFG Expr (Map.fromList [
        (Expr, [[T 'i'], [T '(', N Expr, T ')'], [N Expr, T '+', N Expr], [N Expr, T '*', N Expr]])
        ])
