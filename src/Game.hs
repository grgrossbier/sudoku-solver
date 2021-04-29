module Game where

-- Module for basic types.

import Data.Array.IArray
import Data.Either

n :: Int
n = 9

nn :: ((Int, Int), (Int, Int))
nn = ((1,1),(n,n))

type Cell = Either [Int] Int

type Game = Array (Int, Int) Cell
