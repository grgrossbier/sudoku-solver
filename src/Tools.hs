module Tools where

import Game
import Data.Array
import Data.Maybe
import Data.List.Split
import Data.Either

setList :: [Int]
setList = [1..n]

intToCell :: Int -> Cell
intToCell i = if i <= 0 || i > 9
                then Left []
                else Right i

cellToInt :: Cell -> Int
cellToInt = fromRight 0

printGame :: Game -> IO ()
printGame game = do
    let intList = gameToList game
    mapM_ print intList

gameToList :: Game -> [[Int]]
gameToList game = chunksOf cols nums
  where
    ((r1, c1), (rn,cn)) = bounds game
    cols = cn - c1 + 1
    cells = elems game
    nums = map cellToInt cells

getNewGameFromIO :: IO Game
getNewGameFromIO = undefined 

listToGame :: [[Int]] -> Game
listToGame ints = fmap intToCell intArray
  where
    intArray = listArray nn (concat ints)

isSquare :: [[a]] -> Bool
isSquare ls
    | null ls       = False
    | null firstRow = False
    | otherwise     = all (\row -> length row == length firstRow) ls
  where
    firstRow = head ls

numInRow :: Int -> Game -> [Int]
numInRow row game = foldr eitherAddToList [] eitherRow
  where
    ((_, c1), (_,cn)) = bounds game
    eitherRow = [game ! (row, c) | c <- [c1..cn]]

leftsInRow :: Int -> Game -> [Int]
leftsInRow row game = foldr leftsAddToList [] eitherRow
  where
    ((_, c1), (_,cn)) = bounds game
    eitherRow = [game ! (row, c) | c <- [c1..cn]]

numInCol :: Int -> Game -> [Int]
numInCol col game = foldr eitherAddToList [] eitherCol
  where
    ((r1, _), (rn,_)) = bounds game
    eitherCol = [game ! (r, col) | r <- [r1..rn]]

leftsInCol :: Int -> Game -> [Int]
leftsInCol col game = foldr leftsAddToList [] eitherCol
  where
    ((r1, _), (rn,_)) = bounds game
    eitherCol = [game ! (r, col) | r <- [r1..rn]]

eitherAddToList :: Cell -> [Int] -> [Int]
eitherAddToList e xs = either (const xs) (:xs) e

leftsAddToList :: Cell -> [Int] -> [Int]
leftsAddToList e xs = either (++xs) (const xs) e

numInSquare :: (Int, Int) -> Game -> [Int]
numInSquare loc game = foldr eitherAddToList [] cells
  where
    (bigR, bigC) = iToBigSquareIndex game loc
    x = numOfBigSquareCols game
    littleRange = ( (x*(bigR-1)+1, x*(bigC-1)+1),   (x*bigR, x*bigC) )
    listRange = range littleRange
    cells = map (game !) listRange

leftsInSquare :: (Int, Int) -> Game -> [Int]
leftsInSquare loc game = foldr leftsAddToList [] cells
  where
    (bigR, bigC) = iToBigSquareIndex game loc
    x = numOfBigSquareCols game
    littleRange = ( (x*(bigR-1)+1, x*(bigC-1)+1),   (x*bigR, x*bigC) )
    listRange = range littleRange
    cells = map (game !) listRange
    
iToBigSquareIndex :: Game -> (Int, Int) -> (Int, Int)
iToBigSquareIndex game (row, col) = 
    (((row - 1) `div` bigCols) + 1, ((col - 1) `div` bigCols) + 1)
  where
    bigCols = numOfBigSquareCols game

numOfBigSquareCols :: Game -> Int
numOfBigSquareCols game = round $ sqrt (fromIntegral cols)
  where
    ((_, c1), (_,cn)) = bounds game
    cols = cn - c1 + 1

