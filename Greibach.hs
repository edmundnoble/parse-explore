module Greibach where

import Control.Monad.State
import Control.Lens((&), over, _1)
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- "productive"; all rules contain a terminal first.
-- what the hell is this?
-- it seems straightforward to allow epsilon rules for nonterminals, but what am I
-- doing?
type GRules n t = n -> Map t [n]

-- this isn't really GNF; I require that terminals on the RHS are all different!
data Greibach n t = Greibach n (GRules n t)

data G1 = S | A | B

kenGreibach ::
        Ord t =>
        Greibach n t -> [t] -> ([n], [t])
kenGreibach (Greibach start rules)
        = over _1 (start:) . go [start] where
        go _ [] = ([], [])
        go [] ts = ([], ts)
        go (n:ns) (t:ts) = case Map.lookup t (rules n) of
                Just nes ->
                        over _1 (n:) $ go (nes ++ ns) ts
                _ ->
                        ([], t:ts)
