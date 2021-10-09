module Scratch where

import Game
import Solver
import Tools
import Text.Pretty.Simple
import System.Posix.Internals (puts)
import Data.Either
import Data.List.Split

intBoard1 :: [[Int]]
intBoard1 = [ [ 4,8,9,5,0,1,0,2,0 ]
            , [ 7,5,0,0,0,0,8,1,0 ]
            , [ 0,0,0,0,2,0,5,9,4 ]
            , [ 0,0,8,0,9,0,0,7,5 ]
            , [ 5,0,0,0,0,8,0,0,0 ]
            , [ 0,0,1,0,0,3,0,0,0 ]
            , [ 1,6,0,3,7,4,0,8,2 ]
            , [ 0,0,0,0,0,5,7,3,6 ]
            , [ 0,0,3,0,6,2,4,5,0 ]
            ]

g = listToGame intBoard1

intBoard2 =    [[4,0,0,3,0,0,0,1,0],
                [0,0,0,8,0,0,0,3,2],
                [0,0,5,0,0,0,0,4,0],
                [6,0,9,1,0,0,0,8,0],
                [0,5,4,7,0,8,1,6,0],
                [0,3,0,0,0,6,4,0,7],
                [0,9,0,0,0,0,3,0,0],
                [5,6,0,0,0,1,0,0,0],
                [0,4,0,0,0,3,0,0,8]]
g2 = listToGame intBoard2

intb3 = [[0 ,0 ,7 ,0 ,9 ,4 ,0 ,0 , 0],
         [4 ,0 ,5 ,0 ,0 ,8 ,2 ,0 , 0],
         [0 ,0 ,2 ,0 ,6 ,0 ,0 ,7 , 0],
         [8 ,0 ,0 ,0 ,0 ,7 ,0 ,0 ,5],
         [0 ,5 ,0 ,0 ,0 ,0 ,0 ,3 , 0],
         [9 ,0 ,0 ,5 ,0 ,0 ,0 ,0 ,7],
         [0 ,6 ,0 ,0 ,3 ,0 ,7 ,0 , 0],
         [0 ,0 ,8 ,7 ,0 ,0 ,6 ,0 ,2],
         [0 ,0 ,0 ,8 ,1 ,0 ,3 ,0 ,0]]
g3 = listToGame intb3

intHardBoard = [[5,3,0,6,7,0,9,8,0],
                [0,0,9,0,0,0,2,0,0],
                [8,0,0,0,0,0,0,0,3],
                [4,0,6,0,0,0,8,0,0],
                [0,1,0,3,0,0,0,4,5],
                [0,0,0,0,0,0,0,0,6],
                [0,0,5,0,0,0,0,0,0],
                [9,6,2,0,1,0,3,0,8],
                [0,0,0,9,2,8,0,0,0]]
gH = listToGame intHardBoard

intHardExpert1 = [[8,0,0,0,5,0,0,0,0],
                  [4,0,0,0,0,0,9,1,0],
                  [0,7,0,1,0,0,0,0,0],
                  [0,0,0,0,0,8,7,0,4],
                  [0,0,0,5,3,2,0,0,0],
                  [0,0,0,0,0,7,6,0,0],
                  [0,0,5,8,0,0,0,0,2],
                  [0,0,0,0,0,0,0,0,3],
                  [0,3,2,0,0,0,0,7,0]]
gE1 = listToGame intHardExpert1

intHardExpert2 = [[0,0,0,9,0,0,0,1,0],
                  [3,0,7,0,0,4,9,0,0],
                  [2,0,0,0,0,0,0,0,0],
                  [0,2,0,0,0,0,7,0,0],
                  [7,0,8,0,3,0,0,5,0],
                  [0,1,0,0,0,8,0,0,0],
                  [0,0,0,0,5,0,0,0,6],
                  [9,0,4,0,0,3,1,0,0],
                  [0,8,0,0,0,0,0,0,0]]
gE2 = listToGame intHardExpert2

solveAndPrint :: Game -> IO ()
solveAndPrint g
    | isLeft eitherSolvedOrNot = do
        printit
    | otherwise = do
        printit
  where
    eitherSolvedOrNot = solveGame g
    printit = either printGame printGame eitherSolvedOrNot


{-

TODO: 
    Does this thing solve anything?
    How walk through solver and comment

    Finish guess and check mechanic
        Track guesses [(row, col, guess)]
    r-> If 'no where to go', remove last guess (LIFO) and add to no go pile
    |       'no where to go' - no easy fill and no guesses remaining
    |       Do we have a no-go pile?
    |   After backing out a bad guess, try a new guess from that spot.
    --- If no guesses remaining and 'no where to go'


-}

readAsGame :: String -> Game
readAsGame str = listToGame $ map (\x -> read ("["++x++"]")) (lines str)

readGamesFromFile :: String -> IO [Game]
readGamesFromFile fname = do
    let filePath = "C:\\Users\\Cocytus\\Dropbox\\Code\\sudoku-solver\\data\\"
        fullPath = filePath ++ fname
    fLines <- lines <$> readFile fullPath
    let withCommas = filter (',' `elem`) fLines :: [String]
        splitEvery9 = chunksOf 9 withCommas :: [[String]]
        games = map (readAsGame . unlines) splitEvery9
    return games

gms :: IO [Game]
gms = readGamesFromFile "sudoku_hard.txt"