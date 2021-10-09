module Tools where

import Game
import Data.Array
import Data.Maybe
import Data.List.Split
import Data.Either

setList :: [Int]
setList = [1..n]

newCell :: Cell
newCell = Cell
    { cAnswer = Nothing
    , cPossible = []
    , cImpossible = []}

intToCell :: Int -> Cell
intToCell i = if i <= 0 || i > 9
                then newCell
                else newCell {cAnswer = Just i}

cellToInt :: Cell -> Int
cellToInt cell = fromMaybe 0 $ cAnswer cell

printGame :: Game -> IO ()
printGame game = do
    putStrLn "---------------"
    let intList = gameToList $ gBoard game
    printWithDividers intList
    putStrLn "--\nNotes:"
    putStrLn $ "--\nGuess Tracking:" ++ show (gRecentGuesses game)
    print $ gNote game
    putStrLn "---------------"

printGame' :: (Cell -> [Int]) -> Game -> IO ()
printGame' accessor game = do
    let ((r1, c1), (rn,cn)) = bounds $ gBoard game
        cols = cn - c1 + 1
        cells = elems $ gBoard game
        ints = map accessor cells
        intListList = chunksOf cols ints
    printWithDividers intListList

printWithDividers :: Show a => [[a]] -> IO ()
printWithDividers xss
    | length xss /= n = error "Unexpected size."
    | otherwise = let
        convertToStr = map (map show) xss
        longestStr = maximum $ map length $ concat convertToStr
        padStr str = if length str < longestStr
                     then str ++ replicate (longestStr - length str) ' '
                     else str
        padded = map (map padStr) convertToStr
        verticalBarAt6 xs = insertAt (2*n `div` 3) "|" xs
        verticalBarAt3 xs = insertAt (n `div` 3) "|" xs
        verticalBars = map (verticalBarAt3 . verticalBarAt6) padded
        divider = replicate n $ replicate longestStr '-'
        dividerWithVertical = (verticalBarAt3 . verticalBarAt6) divider
        horizontalBars = ( (insertAt 3 dividerWithVertical)
                         . (insertAt 6 dividerWithVertical)
                         )
                         verticalBars
      in
        mapM_ print (map concat horizontalBars)

insertAt :: Int -> a -> [a]-> [a]
insertAt z y xs = (take z xs) ++ y:(drop z xs)   

gameToList :: Board -> [[Int]]
gameToList b = chunksOf cols nums
  where
    ((r1, c1), (rn,cn)) = bounds b
    cols = cn - c1 + 1
    cells = elems b
    nums = map cellToInt cells

getNewGameFromIO :: IO Game
getNewGameFromIO = undefined 

listToGame :: [[Int]] -> Game
listToGame ints = Game
    { gBoard = board
    , gRecentGuesses = []
    , gNote = GameNotes
        { gnNote = ""
        , gnGuessCount = 0
        , gnBackOutCount = 0
        , gnRecord = []
        }
    }
  where
    intArray = listArray nn (concat ints)
    board = fmap intToCell intArray

isSquare :: [[a]] -> Bool
isSquare ls
    | null ls       = False
    | null firstRow = False
    | otherwise     = all (\row -> length row == length firstRow) ls
  where
    firstRow = head ls

numInRow :: Int -> Game -> [Int]
numInRow row game = foldr answersAddToList [] eitherRow
  where
    board = gBoard game
    ((_, c1), (_,cn)) = bounds board
    eitherRow = [board ! (row, c) | c <- [c1..cn]]

leftsInRow :: Int -> Game -> [Int]
leftsInRow row game = foldr leftsAddToList [] eitherRow
  where
    board = gBoard game
    ((_, c1), (_,cn)) = bounds board
    eitherRow = [board ! (row, c) | c <- [c1..cn]]

numInCol :: Int -> Game -> [Int]
numInCol col game = foldr answersAddToList [] eitherCol
  where
    board = gBoard game
    ((r1, _), (rn,_)) = bounds board
    eitherCol = [board ! (r, col) | r <- [r1..rn]]

leftsInCol :: Int -> Game -> [Int]
leftsInCol col game = foldr leftsAddToList [] eitherCol
  where
    board = gBoard game
    ((r1, _), (rn,_)) = bounds board
    eitherCol = [board ! (r, col) | r <- [r1..rn]]

answersAddToList :: Cell -> [Int] -> [Int]
answersAddToList cell xs = maybe xs (:xs) $ cAnswer cell

leftsAddToList :: Cell -> [Int] -> [Int]
leftsAddToList cell xs =  (++xs) $ cPossible cell

numInSquare :: (Int, Int) -> Game -> [Int]
numInSquare loc game = foldr answersAddToList [] cells
  where
    (bigR, bigC) = iToBigSquareIndex game loc
    x = numOfBigSquareCols game
    littleRange = ( (x*(bigR-1)+1, x*(bigC-1)+1),   (x*bigR, x*bigC) )
    listRange = range littleRange
    cells = map (gBoard game !) listRange

leftsInSquare :: (Int, Int) -> Game -> [Int]
leftsInSquare loc game = foldr leftsAddToList [] cells
  where
    (bigR, bigC) = iToBigSquareIndex game loc
    x = numOfBigSquareCols game
    littleRange = ( (x*(bigR-1)+1, x*(bigC-1)+1),   (x*bigR, x*bigC) )
    listRange = range littleRange
    cells = map (gBoard game !) listRange
    
iToBigSquareIndex :: Game -> (Int, Int) -> (Int, Int)
iToBigSquareIndex game (row, col) = 
    (((row - 1) `div` bigCols) + 1, ((col - 1) `div` bigCols) + 1)
  where
    bigCols = numOfBigSquareCols game

numOfBigSquareCols :: Game -> Int
numOfBigSquareCols game = round $ sqrt (fromIntegral cols)
  where
    ((_, c1), (_,cn)) = bounds $ gBoard game
    cols = cn - c1 + 1

printEither :: Either Game Game -> IO ()
printEither game = either printLeft printRight game
  where
    printLeft game = do
        putStrLn "NOT SOLVED"
        printGame game
    printRight game = do
        putStrLn "SSSOOOOOLLLLVVVEEEDDD!!!!!!"
        printGame game
    
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

isEmptyCell :: Cell -> Bool
isEmptyCell = isNothing . cAnswer

addNote :: String -> Game -> Game
addNote str g = g { gNote = (gNote g) { gnNote = str } }

modGuessCount :: (Int -> Int) -> GameNotes -> GameNotes
modGuessCount f gn = let
    oldCt = gnGuessCount gn
    modCt = f oldCt
  in
    gn { gnGuessCount = modCt }

modBackOutCount :: (Int -> Int) -> GameNotes -> GameNotes
modBackOutCount f gn = let
    oldCt = gnBackOutCount gn
    modCt = f oldCt
  in
    gn { gnBackOutCount = modCt }

resetRecord :: Game -> Game
resetRecord g = let
    oldNote = gNote g
    newNote = oldNote { gnRecord = [] }
  in 
    g { gNote = newNote }

addToRecord :: ((Int, Int), Int, String) -> GameNotes -> GameNotes
addToRecord x gn = gn { gnRecord = x : gnRecord gn }

modRecord 
    :: ([((Row, Col), Int, String)] -> [((Row, Col), Int, String)])
    -> GameNotes 
    -> GameNotes
modRecord f gn = let
    old = gnRecord gn
    mod = f old
  in
    gn { gnRecord = mod }