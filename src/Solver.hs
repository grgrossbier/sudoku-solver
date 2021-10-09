module Solver where

import Data.Either
import Data.Array
import qualified Data.Array.IArray as IA
import Data.List
import Data.Maybe

import Tools
import Game
import Game (GameNotes(gnRecord))

{-

Notes:
 - Create Full Map of Possibilites
 - Check For Solos (one possible in a cell)
 - Check For Onlys (only instance in a row/col/square)
    - Comb through full game. If any occured, restart with a new map of possibilites.
    - If nothing happened - Too Hard. Flag and Quit. 

 - Print possibilities and board nicely.
 - Check basic solver.
 - Keep list of removals. Remap, but then remove the knowns baddies.

 - Create possibilities
 - Start solving
 - Once out of easy solving, start guessing
    - Guess
    - Start solving
    - Guess
    - Start Solving
    - No Guesses Available?
        - REGUESS:
        - Back out last guess, add to list of impossible, and reguess and continue.
    - No reguess available?
        - Reset cImpossible in last guess cell.
        - Then REGUESS one guess before that.

    - When I send it back to solve... could it be invalid?
-}

startSolver :: Game -> Either Game Game
startSolver game
    | (isInvalid . mapOutPossibilities) game  = 
        Left $ addNote "Game was invalid."  game
    | (isInvalid . mapOutPossibilities) finalGame = 
        Left $ addNote "Game was found to be invalid." finalGame
    | otherwise = solveGame game
  where
    maxIter = 81
    solverOut = solver maxIter game
    finalGame = last solverOut 


solveGame :: Game -> Either Game Game
solveGame game
    | null solverOut        = error "Something went wrong with the solver"
    | somethingIsSolved         = Right $ addNote "SUCCESS!" finalGame
    | otherwise                 = maybe
                                  (Left $ addNote "Game was evenutally invalid." mappedFinal) 
                                  solveGame 
                                  guessAdded 
  where
    resetRecordGame = game -- resetRecord game
    mappedGame = mapOutPossibilities resetRecordGame
    maxIter = 81
    solverOut = solver maxIter resetRecordGame
    finalGame = last solverOut
    mappedFinal = mapOutPossibilities finalGame
    somethingIsSolved = isSolved mappedFinal
    guessAdded = guessNextCell mappedFinal

solver :: Int -> Game -> [Game]
solver limit gameIn = gamesOut
  where
    gamesOut = take limit $ iterate mapAndFill gameIn

-- | Look for a guess, if one doesn't exist
guessNextCell :: Game -> Maybe Game
guessNextCell game = let
    remapped = mapOutPossibilities game
    nextGuess = getNextGuess game
  in 
    if isInvalid remapped
    then guessAgain remapped    
    else case nextGuess of
        Just guess -> Just $ setGuess guess remapped
        Nothing -> guessAgain remapped

guessAgain :: Game -> Maybe Game
guessAgain g
    | isNothing lastGuess = Nothing
    | otherwise = gameWithNewGuess 
  where
    game = mapOutPossibilities g
    lastGuess = safeHead $ gRecentGuesses game -- Pull last guess
    guessRemoved = maybe (error "lastGuess was Nothing") 
                         (removeGuess game) -- Remove guess, and add last guess to impossible list
                         lastGuess
    nextGuess = getNextGuess guessRemoved -- Guessable cells occur in order so the same cell will be guessed on again.
    gameWithNewGuess = case nextGuess of -- if no guessable answers remaining in that cell, reset
        Just guess -> Just $ setGuess guess guessRemoved
        Nothing -> guessAgain $ maybe (error "lastGuess was Nothing")
                                      (resetCellAfterBadGuess guessRemoved)
                                      lastGuess

-- | Go to target cell, reset cAnswer, and cImpossible.
resetCellAfterBadGuess :: Game -> ((Row, Col), Int) -> Game
resetCellAfterBadGuess game (index, _) = let
    cellIn = gBoard game ! index
    cellOut = cellIn { cImpossible = [] } -- No need to update cAnswer, that is already set to nothing
    modRecord = addToRecord ( index 
                            , 0
                            , "Reset Impossible."
                            ) 
                            $ gNote game  
  in
    game { gBoard = gBoard game // [(index, cellOut)]
         , gNote = modRecord }
    
getNextGuess :: Game -> Maybe ((Int, Int), Int)
getNextGuess game
    | null firstCellPossibilities = Nothing
    | otherwise = Just (index, head firstCellPossibilities)
  where
    cellList = assocs $ gBoard (mapOutPossibilities game)
    (index, cell) = head $ filter (\(i,c) -> isNothing $ cAnswer c) cellList
    firstCellPossibilities = cPossible cell \\ cImpossible cell

-- | Sets the guess on the board and adds guess to recent guess stack
setGuess :: ((Int, Int), Int) -> Game -> Game
setGuess (index, val) game = let 
    cellIn = gBoard game ! index
    cellOut = cellIn { cAnswer = Just val }
    oldNote = gNote game
    modGuessCt = modGuessCount (+1) oldNote
    modRecord = addToRecord ( index 
                            , val
                            , "Guessed."
                            ) 
                            modGuessCt
  in
    game { gBoard = gBoard game // [(index, cellOut)] 
         , gRecentGuesses = (index, val) : gRecentGuesses game
         , gNote = modRecord}

isInvalid :: Game -> Bool
isInvalid game = any null cPossibleLists
  where
    cells = elems $ gBoard $ mapOutPossibilities game
    cPossibleLists = map cPossible (filter (isNothing . cAnswer) cells)
    
-- TODO - remove guess and add to cImpossible
removeGuess :: Game -> ((Int, Int), Int) -> Game
removeGuess game (index, val) = let
    rewound = rewindUpToSpot game (index, val)
    cellIn = gBoard rewound ! index
    cellOut = cellIn { cAnswer = Nothing
                     , cImpossible = val : cImpossible cellIn }
    oldNote = gNote rewound
    modBackOut = modBackOutCount (+1) oldNote
    modRecord = addToRecord ( index 
                            , 0
                            , "(" ++ show val ++ ")-Impossible-Removed." 
                            ) 
                            modBackOut
  in
    rewound { gBoard = gBoard rewound // [(index, cellOut)] 
            , gRecentGuesses = tail $ gRecentGuesses rewound
            , gNote = modRecord}

rewindUpToSpot :: Game -> ((Int, Int), Int) -> Game
rewindUpToSpot game (index, _) = until (lastRecordFromHere index) undoLastRecord game
  where 
    fst3 (x,_,_) = x
    lastRecordFromHere i = (==i) . fst3 . head . gnRecord . gNote

undoLastRecord :: Game -> Game
undoLastRecord game = let
    (index, _, _) = (head . gnRecord . gNote) game
    cellIn = gBoard game ! index
    cellOut = cellIn { cAnswer = Nothing }
  in 
    game { gBoard = gBoard game // [(index, cellOut)]
         , gNote = modRecord tail $ gNote game}


mapAndFill :: Game -> Game
mapAndFill game = game''''
  where
    game' = mapOutPossibilities game
    game'' = fillInSolos game'
    game''' = mapOutPossibilities game''
    game'''' = fillInOnlys game'''

mapOutPossibilities :: Game -> Game
mapOutPossibilities game = foldr loadPossibilitiesIntoGame game possiblesByIndex
  where
    possiblesByIndex = map (possibilitiesForCell game) (range nn)


fillInSolos :: Game -> Game
fillInSolos game = foldr flipSoloThenUpdate game (range nn)

flipSoloThenUpdate :: (Int, Int) -> Game -> Game
flipSoloThenUpdate index game
    | isNothing flippedCell = game
    | otherwise = placeAnswerAndClear "Solo." index solo game
  where 
    flippedCell = flipSolo $ (gBoard game) ! index
    solo = fromJust flippedCell

flipSolo :: Cell -> Maybe Int
flipSolo cell
    | isJust (cAnswer cell) = Nothing
    | length (cPossible cell) == 1 = Just $ head $ cPossible cell
    | otherwise = Nothing

fillInOnlys :: Game -> Game
fillInOnlys game = foldr checkForOnlyThenLoad game $ range nn

isSolved :: Game -> Bool
isSolved game = not $ any isEmptyCell $ elems $ gBoard game

possibilitiesForCell :: Game -> (Int, Int) -> ((Int, Int), [Int])
possibilitiesForCell game index@(row, col)
    | isJust (cAnswer cellIn) = (index, [])
    | otherwise = (index, ((setList \\ rowNums) \\ colNums) \\ sqrNums)
  where
    cellIn = gBoard game ! index
    rowNums = numInRow row game
    colNums = numInCol col game
    sqrNums = numInSquare index game

loadPossibilitiesIntoGame :: ((Int, Int), [Int]) -> Game -> Game
loadPossibilitiesIntoGame (index, xs) game = let
    cellIn = gBoard game ! index
    cellOut = cellIn { cPossible = xs }
  in 
    game { gBoard = gBoard game // [(index, cellOut)] }

checkForOnlyThenLoad :: (Int, Int) -> Game -> Game
checkForOnlyThenLoad index@(row, col) game
    | isJust firstTrueIndex = 
        placeAnswerAndClear "Only." index (possibles !! fromJust firstTrueIndex) game
    | otherwise = game
  where
    possibles = cPossible (gBoard game ! index)
    rowLefts = leftsInRow row game \\ possibles
    colLefts = leftsInCol col game \\ possibles
    sqrLefts = leftsInSquare index game \\ possibles
    isOnly li = not $ all (li `elem`) [rowLefts, colLefts, sqrLefts]
    eachOnlyBool = map isOnly possibles
    firstTrueIndex = elemIndex True eachOnlyBool

placeAnswerAndClear :: String -> (Int, Int) -> Int -> Game -> Game
placeAnswerAndClear note index@(row, col) num game = removedFromSqr
  where
    targetCell = gBoard game ! index
    cellWithAnswer = targetCell { cAnswer = Just num }
    modRecord = addToRecord ( index 
                            , num
                            , note 
                            ) 
                            $ gNote game
    answerPlaced = game { gBoard = gBoard game // [(index, cellWithAnswer)] 
                        , gNote = modRecord
                        }
    removedFromRows = applyToRow removePossibility row num answerPlaced
    removedFromCols = applyToCol removePossibility col num removedFromRows
    removedFromSqr = applyToSquare removePossibility (row,col) num removedFromCols

removePossibility :: ((Int, Int), Int) -> Game -> Game
removePossibility (index, val) game = let
    cellIn = gBoard game ! index
    cellOut = cellIn { cPossible = cPossible cellIn \\ [val] }
  in
    game { gBoard = gBoard game // [(index, cellOut)] }

applyToRow :: (((Int, Int), Int) -> Game -> Game) -> Int -> Int -> Game -> Game
applyToRow f row item game = foldr applyTo game [1..n]
  where
    applyTo col = f ((row, col), item)

applyToCol :: (((Int, Int), Int) -> Game -> Game) -> Int -> Int -> Game -> Game
applyToCol f col item game = foldr applyTo game [1..n]
  where
    applyTo row = f ((row, col), item)

applyToSquare :: (((Int, Int), Int) -> Game -> Game) -> (Int, Int) -> Int -> Game -> Game
applyToSquare f loc item game = foldr applyTo game listRange
  where
    applyTo index = f (index, item)
    (bigR, bigC) = iToBigSquareIndex game loc
    x = numOfBigSquareCols game
    littleRange = ( (x*(bigR-1)+1, x*(bigC-1)+1),   (x*bigR, x*bigC) )
    listRange = range littleRange

{-

Why isn't it finishing the solve? Likely guessing and not backing out correctly.
Double check basic fill - spot check, then watch how it handles bad guesses

Add guess counter to see if it's even guessing, and back out counter

-}