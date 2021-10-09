module Tools where

import Game
import Data.Array
import Data.Maybe
import Data.List.Split
import Data.Either

setList :: [Int]
setList = [1..n]

-- | Simple cell with no information.
newCell :: Cell
newCell = Cell
    { cAnswer = Nothing
    , cPossible = []
    , cImpossible = []}

-- | Turn an int into a cell
intToCell :: Int -> Cell
intToCell i = if i <= 0 || i > 9
                then newCell
                else newCell {cAnswer = Just i}

-- | Turn an Cell into an Int
cellToInt :: Cell -> Int
cellToInt cell = fromMaybe 0 $ cAnswer cell

-- | Print the game to the command line with accompanying information.
printGame :: Game -> IO ()
printGame game = do
    putStrLn "---------------"
    let intList = gameToList $ gBoard game
    printWithDividers intList
    putStrLn "--\nNotes:"
    putStrLn $ "--\nGuess Tracking:" ++ show (gRecentGuesses game)
    print $ gNote game
    putStrLn "---------------"

-- | Print a different accessor's information within a cell to the command line.
printGame' :: Show a => (Cell -> a) -> Game -> IO ()
printGame' accessor game = do
    let ((r1, c1), (rn,cn)) = bounds $ gBoard game
        cols = cn - c1 + 1
        cells = elems $ gBoard game
        ints = map accessor cells
        intListList = chunksOf cols ints
    printWithDividers intListList

-- | Prints a number board more asthetically by adding dividers.
--   Caution - THIS FUNCTION ASSUMES A 9x9 GRID.
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

-- | Insert a value into a list. UNSAFE!
insertAt :: Int -> a -> [a]-> [a]
insertAt z y xs = (take z xs) ++ y:(drop z xs)   

-- | Used for printing. 
gameToList :: Board -> [[Int]]
gameToList b = chunksOf cols nums
  where
    ((r1, c1), (rn,cn)) = bounds b
    cols = cn - c1 + 1
    cells = elems b
    nums = map cellToInt cells

-- | Used to create a new game from user defined sources. 
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

-- | Ensure the board is a valid sudoku shape (square) -- TODO, is it 
--   also a 'square' (4,9,16,25...) ???
isSquare :: [[a]] -> Bool
isSquare ls
    | null ls       = False
    | null firstRow = False
    | otherwise     = all (\row -> length row == length firstRow) ls
  where
    firstRow = head ls

-- | Collect all the cell in one row and add the numbers contained in them to
--   a list.  
numInRow :: Int -> Game -> [Int]
numInRow row game = foldr answersAddToList [] eitherRow
  where
    board = gBoard game
    ((_, c1), (_,cn)) = bounds board
    eitherRow = [board ! (row, c) | c <- [c1..cn]]

-- | Collect all the cell in one row and add the cPossible in them to
--   a list. 
possibleInRow :: Int -> Game -> [Int]
possibleInRow row game = foldr leftsAddToList [] eitherRow
  where
    board = gBoard game
    ((_, c1), (_,cn)) = bounds board
    eitherRow = [board ! (row, c) | c <- [c1..cn]]

-- | Collect all the cell in one column and add the numbers contained in them to
--   a list.  
numInCol :: Int -> Game -> [Int]
numInCol col game = foldr answersAddToList [] eitherCol
  where
    board = gBoard game
    ((r1, _), (rn,_)) = bounds board
    eitherCol = [board ! (r, col) | r <- [r1..rn]]

-- | Collect all the cell in one row and add the cPossible in them to
--   a list. 
possibleInCol :: Int -> Game -> [Int]
possibleInCol col game = foldr leftsAddToList [] eitherCol
  where
    board = gBoard game
    ((r1, _), (rn,_)) = bounds board
    eitherCol = [board ! (r, col) | r <- [r1..rn]]

-- | Used for a fold. Given a cell and a list, add the cAnswer to 
--   the list if not nothing.
answersAddToList :: Cell -> [Int] -> [Int]
answersAddToList cell xs = maybe xs (:xs) $ cAnswer cell

-- | Used for a fold. Given a cell and a list, add the cPossible to 
--   the list.
leftsAddToList :: Cell -> [Int] -> [Int]
leftsAddToList cell xs = (++xs) $ cPossible cell

-- | Collect all the cell in one square and add the numbers contained in them to
--   a list.  
numInSquare :: (Int, Int) -> Game -> [Int]
numInSquare loc game = foldr answersAddToList [] cells
  where
    (bigR, bigC) = iToBigSquareIndex game loc
    x = numOfBigSquareCols game
    littleRange = ( (x*(bigR-1)+1, x*(bigC-1)+1),   (x*bigR, x*bigC) )
    listRange = range littleRange
    cells = map (gBoard game !) listRange

-- | Collect all the cell in one square and add the cPossible in them to
--   a list.  
possibleInSquare :: (Int, Int) -> Game -> [Int]
possibleInSquare loc game = foldr leftsAddToList [] cells
  where
    (bigR, bigC) = iToBigSquareIndex game loc
    x = numOfBigSquareCols game
    littleRange = ( (x*(bigR-1)+1, x*(bigC-1)+1),   (x*bigR, x*bigC) )
    listRange = range littleRange
    cells = map (gBoard game !) listRange

-- | Given a cell position and the game, return the 'big square' location.
--   Cell index 2,2 is in big square 1,1    
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

-- | TODO - use this more!    
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- | Is cAnswer nothing?
isEmptyCell :: Cell -> Bool
isEmptyCell = isNothing . cAnswer

-- | Add a note to (gnNote . gNote)
addNote :: String -> Game -> Game
addNote str g = g { gNote = (gNote g) { gnNote = str } }

-- | Applies a function to gnGuessCount. Usually used to (+1) the number.
modGuessCount :: (Int -> Int) -> GameNotes -> GameNotes
modGuessCount f gn = let
    oldCt = gnGuessCount gn
    modCt = f oldCt
  in
    gn { gnGuessCount = modCt }

-- | Applies a function to gnBackOutCount. Usually used to (+1) the number.
modBackOutCount :: (Int -> Int) -> GameNotes -> GameNotes
modBackOutCount f gn = let
    oldCt = gnBackOutCount gn
    modCt = f oldCt
  in
    gn { gnBackOutCount = modCt }

-- | Adds information to gnRecord
addToRecord :: ((Int, Int), Int, String) -> GameNotes -> GameNotes
addToRecord x gn = gn { gnRecord = x : gnRecord gn }

-- | Modifies a record used a function. Usually used to help 'pop' the 'head'
--   off the record when back tracking after a bad guess. 
modRecord 
    :: ([((Row, Col), Int, String)] -> [((Row, Col), Int, String)])
    -> GameNotes 
    -> GameNotes
modRecord f gn = let
    old = gnRecord gn
    mod = f old
  in
    gn { gnRecord = mod }