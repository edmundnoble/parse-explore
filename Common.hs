{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}
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

type CFRules n t = Map n [[Either n t]]

data CFG n t = CFG n (CFRules n t)
        deriving (Show)

type AnnotCFRules a n t = Map n (a, [[Either n t]])

data AnnotCFG n t a = ACFG n (AnnotCFRules a n t)
        deriving (Functor, Show)

data PTree n t = PNode n [PTree n t] | PLeaf t
        deriving (Show)

data PAnd n t = PAnd Int [PForest n t] | PAndLeaf t
        deriving (Show)

data PForest n t = POr n [PAnd n t] | PForestLeaf t
        deriving (Show)

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
                maybe True (<= len) mi && maybe True (>= len) ma && checkblum ts bl

cfgStart :: CFG n t -> n
cfgStart (CFG s _) = s

ruleCFG :: Ord n => CFG n t -> n -> [[Either n t]]
ruleCFG (CFG _ rs) n = fromJust (Map.lookup n rs)

ruleACFG :: Ord n => AnnotCFG n t a -> n -> (a, [[Either n t]])
ruleACFG (ACFG _ rs) n = fromJust (Map.lookup n rs)

shortestRulesFirst :: CFG n t -> CFG n t
shortestRulesFirst (CFG start rules) =
        CFG start $ fmap (sortOn length) rules

{-# complete T, N #-}

pattern T t = Right t
pattern N n = Left n

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
arithSampleBeeg = go 4 where
        go 0 = "i"
        go n = "(" ++ go (n - 1) ++ "+" ++ go (n - 1) ++ ")"

arithAmbiGram = shortestRulesFirst $ CFG Expr (Map.fromList [
        (Expr, [[T 'i'], [T '(', N Expr, T ')'], [N Expr, T '+', N Expr], [N Expr, T '*', N Expr]])
        ])
