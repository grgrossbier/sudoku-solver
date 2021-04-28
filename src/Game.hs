module Game where

-- Module for basic types.

import Data.Array

n :: Int
n = 9

nn :: ((Int, Int), (Int, Int))
nn = ((1,1),(n,n))

type Cell = Maybe Int

type Game = Array (Int, Int) Cell

getNewGameFromIO :: IO Game
getNewGameFromIO = undefined 

listToGame :: [[Int]] -> Game
listToGame ints = fmap intToCell intArray
  where
    intArray = listArray nn (concat ints)
    intToCell i = if i <= 0 || i > 9
                  then Nothing
                  else Just i

isSquare :: [[a]] -> Bool
isSquare ls
    | null ls       = False
    | null firstRow = False
    | otherwise     = all (\row -> length row == length firstRow) ls
  where
    firstRow = head ls