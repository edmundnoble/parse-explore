{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# language TupleSections #-}

module Common where

import Data.Map(Map)
import Data.Maybe(fromJust)
import qualified Data.Map as Map

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

-- annotMinLength :: AnnotCFG a n t -> AnnotCFG n

cfgStart :: CFG n t -> n
cfgStart (CFG s _) = s

ruleCFG :: Ord n => CFG n t -> n -> [[Either n t]]
ruleCFG (CFG _ rs) n = fromJust (Map.lookup n rs)

{-# complete T, N #-}

pattern T t = Right t
pattern N n = Left n

lit fa = fmap Right fa

data TDH
        = Name
        | Sentence
        | List
        deriving (Eq, Ord, Show)

tdhGram = CFG Sentence (Map.fromList [
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

arithGram = CFG Expr (Map.fromList [
        (Expr, [[N Term], [N Expr, T '+', N Term]]),
        (Term, [[N Factor], [N Term, T '*', N Factor]]),
        (Factor, [[T 'i'], [T '(', T 'i', T ')']])
        ])

arithSample1 = "(i+i)*i"
arithSample2 = "i"
arithSample3 = "i+i"
