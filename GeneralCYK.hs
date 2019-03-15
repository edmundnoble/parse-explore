-- "general" CYK parser. Doesn't need CNF but doesn't handle epsilon-rules.
{-# language EmptyCase #-}
{-# language ViewPatterns #-}
{-# language ScopedTypeVariables #-}

module GeneralCYK where

import qualified Data.Map as Map
import Data.Map(Map)

import Grammar
import Types

type CYKChart n t = [[Either [n] t]]

start :: [t] -> CYKChart n t
start = fmap PLeaf

-- a specialization of `repeatedly :: a -> (a -> Maybe a) -> a`
listyfix :: (t -> [a]) -> ([a] -> t -> t) -> t -> t
listyfix f g a = case f a of
        [] -> a
        ns -> listyfix f g (g ns a)

runUnits :: forall n t. (Eq n, Eq t) => CFG n t -> CYKChart n t -> CYKChart n t
runUnits (CFG _ rules) chart = runUnit <$> chart
        where
        findUnits :: Either n t -> [n]
        findUnits nt = fst <$> (filter (any (== [nt]) . snd) $ Map.toList rules)

        findTreeUnits :: PTree [n] t -> [n]
        findTreeUnits (PLeaf t) = findUnits (Right t)
        findTreeUnits (PNode ns ch) = findUnits . Left =<< ns

        singletonNode ns ch = PNode ns [ch]

        runUnit :: PTree [n] t -> PTree [n] t
        runUnit = listyfix findTreeUnits singletonNode
