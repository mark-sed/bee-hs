{- |
Module     : bee.hs
Description: Bee language compiler
Author     : Marek Sedlacek
Email      : mr.mareksedlacek@gmail.com
Date       : April 2022
Copyright  : 2022 Marek Sedlacek. All rights reserved.

Bee language compiler written in haskell.
The compiler compiles down to Ebel language.
-}

module Main where

import System.Environment
import Data.Char
import Parser

compile _ = "Bzzz" --TODO: Always add requires 0.3.1 since floats might be used... or even check for exact min version needed.

-- | Entry point
-- | Takes the code as an argument
main :: IO()
main = do
    args <- getArgs
    putStr $ compile (head args)

