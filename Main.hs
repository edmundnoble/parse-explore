module Main where

import Common
import Unger

main :: IO ()
main = print $ heurUnger (compoundAnnot arithGram) arithSampleBeeg
