module Earley where

import Common
import Types

-- use a splay tree to store chart lines?

-- data SList a = SNil | SCons !a !(SList a)

-- I use start/length not start/end
data InProgressEntry n t = InProgressEntry !Int !Int !n ![Either n t] ![Either n t]

data FinishedEntry n t = FinishedEntry !Int !Int ![Either n t]

data EarleyChart n t = EarleyChart [FinishedEntry n t] [InProgressEntry n t]

emptyChart :: Ord n => CFG n t -> EarleyChart n t
emptyChart cfg@(CFG start _) = let
        st = ruleCFG cfg start
        in undefined
