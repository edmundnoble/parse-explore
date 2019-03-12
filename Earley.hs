{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Earley where

import Control.Lens
import Data.Either(isRight)
import Data.List(partition)
import Data.Maybe(catMaybes, listToMaybe, maybeToList)
import Data.Map.Strict(Map)
import Data.Semigroup(Dual(..), Endo(..))
import Data.Set(Set)
import Debug.Trace

import Common
import Grammar
import Types

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- use a splay tree to store chart lines?

-- data SList a = SNil | SCons !a !(SList a)

-- I use start/length not start/end
data InProgressEntry n t = InProgressEntry !Int !Int !n ![Either n t] ![Either n t]
        deriving (Eq, Ord, Show)

prettyEithers :: PP.Pretty n => [Either n Char] -> PP.Doc
prettyEithers xs = PP.hsep $ either PP.pretty PP.char <$> xs

instance PP.Pretty n => PP.Pretty (InProgressEntry n Char) where
        pretty (InProgressEntry i k n sf rem) =
                PP.hsep [
                        PP.pretty i
                      , PP.pretty k
                      , PP.pretty n
                      , prettyEithers (reverse sf)
                      , PP.text "."
                      , prettyEithers rem
                        ]

data FinishedEntry n t = FinishedEntry !Int !Int !n ![Either n t]
        deriving (Eq, Ord, Show)

instance PP.Pretty n => PP.Pretty (FinishedEntry n Char) where
        pretty (FinishedEntry i k n str) =
                PP.hsep [
                        PP.pretty i
                      , PP.pretty k
                      , PP.pretty n
                      , prettyEithers str
                        ]

data EarleyChart n t = EarleyChart !Int [FinishedEntry n t] (Set (InProgressEntry n t))
        deriving (Eq, Ord, Show)

instance (PP.Pretty n) => PP.Pretty (EarleyChart n Char) where
        pretty (EarleyChart i is fs) =
                PP.vcat [
                        PP.pretty i
                      , PP.vcat (PP.pretty <$> is)
                      , PP.vcat (PP.pretty <$> Set.toList fs)
                        ]

completeSym :: FinishedEntry n t -> n
completeSym (FinishedEntry _ _ n _) = n

inProgressSym :: InProgressEntry n t -> n
inProgressSym (InProgressEntry _ _ n _ _) = n

inProgress :: Lens' (EarleyChart n t) (Set (InProgressEntry n t))
inProgress f (EarleyChart n fs ip) =
        fmap (EarleyChart n fs) (f ip)

remaining :: Lens' (InProgressEntry n t) [Either n t]
remaining f (InProgressEntry n l nt sf rem) =
        fmap (InProgressEntry n l nt sf) (f rem)

closeChart :: EarleyChart n t -> EarleyChart n t
closeChart _ = undefined

maybeToSet :: Ord a => Maybe a -> Set a
maybeToSet = Set.fromList . maybeToList

emptyChart :: (Ord n, Ord t) => CFG n t -> n -> EarleyChart n t
emptyChart cfg begin = let
        lcsn = bindSet (maybeToSet . either Just (const Nothing)) (leftCornerSet (ruleCFG cfg) (Set.singleton begin))
        ruleFromNonterm n = Set.fromList $ InProgressEntry 0 0 n [] <$> ruleCFG cfg n
        in EarleyChart 0 [] $ ruleFromNonterm `bindSet` lcsn

collectUnzipWith :: (a -> Maybe (Either l r)) -> [a] -> ([l], [r])
collectUnzipWith f [] = ([], [])
collectUnzipWith f (a:as) = case f a of
        Nothing ->        collectUnzipWith f as
        Just (Left l) ->  over _1 (l:) $ collectUnzipWith f as
        Just (Right r) -> over _2 (r:) $ collectUnzipWith f as

ts str a = trace (str ++ " : " ++ show a) a

entryFromNonterm cfg i n = Set.fromList $ InProgressEntry i 0 n [] <$> ruleCFG cfg n

inProgressRules is n = (^. remaining) <$> (filter ((== n) . inProgressSym) is)

-- seems like this should be unified with `scanNonterms`
-- to clear up unsatisfiable rules?
scan new (InProgressEntry i' l nt sofar (Right next:rem'))
        | new == next = Just . Left $ InProgressEntry i' l nt (Right next:sofar) rem'
        | otherwise   = Nothing
scan _ ipe = Just $ Right ipe

scanNonterms news (InProgressEntry i' l nt sofar (Left next:rem'))
        | Set.member next news =
                Just . Left $ InProgressEntry i' l nt (Left next:sofar) rem'
scanNonterms news ipe
        = Just (Right ipe)

complete i e@(InProgressEntry i' _ n sf [])
        = Just $ Left $ FinishedEntry i' i n (reverse sf)
complete i e
        = Just (Right e)

propagate :: (Ord a, Ord t) =>
     CFG a t
     -> Int
     -> [InProgressEntry a t]
     -> Set a
     -> ([FinishedEntry a t], [InProgressEntry a t])
propagate cfg i ip news = let
        (advanced, stuck) = collectUnzipWith (scanNonterms news) ip
        (newlyComplete, incomplete) = collectUnzipWith (complete i) advanced
        newNonterms = bindSet (maybeToSet . either Just (const Nothing)) $
                leftCornerSet (inProgressRules stuck) (Set.fromList $ completeSym <$> newlyComplete)
        newEntries = Set.toList $ entryFromNonterm cfg i `bindSet` newNonterms
        in if | null advanced ->
                        ([], [])
                | null newlyComplete ->
                        ([], advanced ++ stuck)
                | otherwise ->
                        over _1 (++ newlyComplete) $
                        over _2 (++ newEntries ++ incomplete) $
                                propagate cfg i stuck (Set.fromList $ completeSym <$> newlyComplete)

feedTerm :: forall n t. (Ord n, Ord t) => CFG n t -> EarleyChart n t -> t -> EarleyChart n t
feedTerm cfg (EarleyChart i cs ips) t = let
        (advanced, stuck) = collectUnzipWith (scan t) (Set.toList ips)
        (newlyComplete, incomplete) = collectUnzipWith (complete i) advanced
        newNonterms = bindSet (maybeToSet . either Just (const Nothing)) $
                leftCornerSet (inProgressRules stuck) (Set.fromList $ completeSym <$> newlyComplete)
        newEntries = Set.toList $ entryFromNonterm cfg i `bindSet` newNonterms
        (newlyComplete', inProgress') =
                if | null advanced ->
                        ([], [])
                        | null newlyComplete ->
                        ([], advanced ++ stuck)
                        | otherwise ->
                        over _1 (++ newlyComplete) $
                        over _2 (++ newEntries ++ incomplete) $
                                propagate cfg i stuck (Set.fromList $ completeSym <$> newlyComplete)
        in EarleyChart (i+1) newlyComplete' (Set.fromList inProgress')

feedAll :: (Ord n, Ord t) => CFG n t -> [t] -> EarleyChart n t -> EarleyChart n t
feedAll gram str =
        appEndo $ getDual $ foldMap (Dual . Endo) $ flip (feedTerm gram) <$> str
