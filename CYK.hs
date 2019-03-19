{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}

module CYK where

import Control.Lens

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map)

import Data.Maybe(catMaybes, fromMaybe)

import Data.List(foldl')
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import GHC.Word(Word8)

import Text.Ascii(ascii)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Grammar
import Types

-- (start,length)
newtype CYKChart n = CYKChart (Map (Int,Int) (NonEmpty n))
        deriving (Eq, Ord, Show)

maybeNel :: Iso' (Maybe (NonEmpty a)) [a]
maybeNel = iso (maybe [] NonEmpty.toList) (\case
        [] -> Nothing
        (x:xs) -> Just $ x :| xs
        )

fromListNel :: [(a, Maybe b)] -> [(a, b)]
fromListNel = catMaybes . fmap sequence

start :: Ord n => CNFCFG n Word8 -> ByteString -> CYKChart n
start (CNFCFG _ _ backwardRules) bs = CYKChart $ let
        locations = (,1) <$> [0..BS.length bs - 1]
        matches = match <$> BS.unpack bs
        locatedMatches = filter (not . null . snd) (locations `zip` matches)
        in Map.fromList $ fromListNel locatedMatches
        where
        match term = Map.lookup (Right term) backwardRules

ts s i = trace (s ++ " " ++ show i) i

-- requirement `l <= BS.length bs`
offlineStep :: (Show n, Ord n) => CNFCFG n Word8 -> ByteString -> CYKChart n -> Int -> CYKChart n
offlineStep (CNFCFG _ _ backwardRules) bs (CYKChart chart) substringLen = CYKChart $ let
        parents = do
                substringStart <- [0..BS.length bs - substringLen]
                let substringEnd = substringStart + substringLen
                partitionLength <- [1..substringLen - 1]
                let lookupAsList k m = Map.lookup k m^.maybeNel
                l <- lookupAsList (substringStart,partitionLength) chart
                r <- lookupAsList (substringStart+partitionLength,substringEnd-substringStart-partitionLength) chart
                let parents = lookupAsList (Left (l,r)) backwardRules
                return $ ((substringStart, substringLen), parents)
        insertParent (k, v) c = maybe c (flip (Map.insert k) c) (v^.(from maybeNel))
        in foldr insertParent chart parents

offlineRecognize :: (Show n, Ord n) => CNFCFG n Word8 -> ByteString -> CYKChart n
offlineRecognize cfg str
        = foldl' (offlineStep cfg str) (start cfg str) [2..BS.length str]

-- Numbers ---> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- Numbers ---> Integer Digit
-- Numbers ---> N1 Scale’ | Integer Fraction
-- N1 ---> Integer Fraction
-- Integer ---> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- Integer ---> Integer Digit
-- Fraction ---> T1 Integer
-- T1 ---> .
-- Scale’ ---> N2 Integer
-- N2 ---> T2 Sign
-- T2 ---> e
-- Digit ---> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- Sign ---> + | -

data Number = Numbers | N1 | Integer | Fraction | T1 | Scale | N2 | T2 | Digit | Sign
        deriving (Eq, Ord, Show)

t = T . ascii

numberGrammar = mkCNFCFG Numbers $ Map.fromList [
        (Numbers, NonEmpty.fromList $
                (t <$> ['0'..'9']) ++
                [N (Integer, Digit), N (N1, Scale), N (Integer, Fraction)]),
        (N1, pure (N (Integer, Fraction))),
        (Integer, NonEmpty.fromList $
                (t <$> ['0'..'9']) ++ [N (Integer, Digit)]),
        (Fraction, pure $ N (T1, Integer)),
        (T1, pure $ t '.'),
        (Scale, pure $ N (N2, Integer)),
        (N2, pure $ N (T2, Sign)),
        (T2, pure $ t 'e'),
        (Digit, NonEmpty.fromList $ t <$> ['0'..'9']),
        (Sign, NonEmpty.fromList $ t <$> ['+', '-'])
        ]

-- impossible :: CYKChart n -> Bool
-- impossible ch = let
--         strLength = length ch

--         in
