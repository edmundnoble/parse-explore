module Main where

import Common
import Unger

import Data.ByteString.Char8

main :: IO ()
main = print $ ungerTrieMemoBS arithGram (pack arithSampleBeeg)
