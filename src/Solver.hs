module Solver where

import Data.Either
import Data.Array
import qualified Data.Array.IArray as IA
import Data.List
import Data.Maybe

import Tools
import Game

{-

Notes:
 - Create Full Map of Possibilites
 - Check For Solos (one possible in a cell)
 - Check For Onlys (only instance in a row/col/square)
    - Comb through full game. If any occured, restart with a new map of possibilites.
    - If nothing happened - Too Hard. Flag and Quit. 

-}

solveGame :: Game -> Game
solveGame game
    | outGameIndex <= 0         = error "Something went wrong with the solver"
    | outGameIndex == maxIter   = last solverOut
    | otherwise                 = solverOut !! (outGameIndex + 1)
  where
    maxIter = 200
    solverOut = solver maxIter game
    outGameIndex = length $ takeWhile (not.isSolved) solverOut

solver :: Int -> Game -> [Game]
solver limit gameIn = gamesOut
  where
    gamesOut = take limit $ iterate mapAndFill gameIn

mapAndFill :: Game -> Game
mapAndFill game = game''''
  where
    game' = mapOutPossibilities game
    game'' = fillInSolos game'
    game''' = mapOutPossibilities game''
    game'''' = fillInOnlys game'''

mapOutPossibilities :: Game -> Game
mapOutPossibilities game = foldr loadResultIntoGame game possiblesByIndex
  where
    possiblesByIndex = map (possibilitiesForCell game) (range nn)


fillInSolos :: Game -> Game
fillInSolos = IA.amap flipSolo

flipSolo :: Cell -> Cell
flipSolo cell
    | isRight cell = cell
    | length left == 1 = Right $ head left
    | otherwise = cell
  where
    left = fromLeft [1,2] cell

fillInOnlys :: Game -> Game
fillInOnlys game = foldr checkForOnlyThenLoad game $ range nn

isSolved :: Game -> Bool
isSolved game = null $ lefts (elems game)

possibilitiesForCell :: Game -> (Int, Int) -> ((Int, Int), [Int])
possibilitiesForCell game (row, col) = ((row, col), 
    ((setList \\ rowNums) \\ colNums) \\ sqrNums)
  where
    rowNums = numInRow row game
    colNums = numInCol col game
    sqrNums = numInSquare (row, col) game

loadResultIntoGame :: ((Int, Int), [Int]) -> Game -> Game
loadResultIntoGame (index, xs) game
    | isRight cell = game
    | isLeft cell = game // [(index, Left xs)]
  where
    cell = game ! index

checkForOnlyThenLoad :: (Int, Int) -> Game -> Game
checkForOnlyThenLoad index@(row, col) game
    | isJust firstTrueIndex = mapOutPossibilities $
        game // [(index, Right (left !! (fromJust firstTrueIndex)))]
    | otherwise = game
  where
    left = fromLeft [] (game ! index)
    rowLefts = leftsInRow row game \\ left
    colLefts = leftsInCol col game \\ left
    sqrLefts = leftsInSquare index game \\ left
    isOnly li = not $ all (li `elem`) [rowLefts, colLefts, sqrLefts]
    eachOnlyBool = map isOnly left
    firstTrueIndex = elemIndex True eachOnlyBool

