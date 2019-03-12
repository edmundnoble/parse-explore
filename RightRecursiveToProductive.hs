{-# language LambdaCase #-}
{-# language ViewPatterns #-}

module RightRecursiveToProductive where

import Control.Monad.State
import Control.Lens((&), over, _1)
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- productive; all rules contain a terminal first.
type PRules n t = n -> Map t (Maybe n)

-- turns out this is just a DFA
data Productive n t = Productive n (PRules n t)

startP :: Productive n t -> n
startP (Productive start _) = start

data L = A | C deriving (Eq, Ord, Show)

testGram = Productive A $ \case
        A -> Map.fromList $
                [('a', Just A)
                ,('b', Just C)]
        C -> Map.fromList $
                [('c', Nothing)]

-- appears to take the trace of a moore machine?
kenProductive ::
        Ord t =>
        Productive n t -> [t] -> ([n], [t])
kenProductive (Productive start rules)
        = over _1 (start:) . go start where
        go n [] = ([], [])
        go n (t:ts) = case Map.lookup t (rules n) of
                Just Nothing ->
                        ([n], ts)
                Just (Just ne) ->
                        over _1 (n:) $ go ne ts
                _ ->
                        ([], t:ts)

data BQueue a = BQueue [a] [a]

enq :: a -> BQueue a -> BQueue a
enq a (BQueue fs bs) = BQueue (a:fs) bs

deq :: BQueue a -> Maybe (a, BQueue a)
deq (BQueue [] []) = Nothing
deq (BQueue fs (b:bs)) = Just (b, BQueue fs bs)
deq (BQueue fs []) = Just (head rev, BQueue [] (tail rev))
        where
        rev = reverse fs

quempty :: BQueue a
quempty = BQueue [] []

-- genProductive :: (Ord n, Ord t) => Productive n t -> Set [t]
-- genProductive (Productive start rules) =
--         go (enq [Left start] quempty)
--         where
--         go :: BQueue (Set [Either n t]) -> Set [t]
--         go q = case deq q of
--                 Nothing -> Set.empty
--                 Just sent -> if (all isRight sent)
--                         then undefined
--                         else undefined
--         exp [] = []
--         exp (Right t:sent') = exp sent'
--         exp (Left n:sent') = exp sent'

expandOnce :: (Ord n, Ord t) => PRules n t -> [Either n t] -> Set [Either n t]
expandOnce rules [] = Set.empty
expandOnce rules (Right t:xs) = Set.map (Right t:) (expandOnce rules xs)
expandOnce rules (Left n:xs) = let
        possibles = Map.toList $ rules n
        loop = expandOnce rules xs
        exp l = Set.fromList (flip traverse possibles (\case
                (t, Just n)  -> Right t:Left n:l
                (t, Nothing) -> Right t:l
                )) in
        Set.unions $ Set.map exp loop

type RRRules n t = n -> [([t], Maybe n)]

data RightRec n t = RightRec n (RRRules n t)

testGramR = RightRec A $ \case
        A ->
                [("a", Just A)
                ,("b", Just C)]
        C ->
                [("c", Nothing)
                ,("b", Just C)]
