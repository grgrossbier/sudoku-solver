module Game where

-- Module for basic types.

import Data.Array.IArray
import Data.Either

-- Standard board size is 9x9
-- Using n as a shortcut.
n :: Int
n = 9

-- Shortcut for setting up the board
nn :: ((Int, Int), (Int, Int))
nn = ((1,1),(n,n))

data Cell = Cell
    { cPossible :: [Int] -- ^ Possible solutions to fill out the cell
    , cImpossible :: [Int] -- ^ Impossible solutions to fill out the cell
    , cAnswer :: Maybe Int -- ^ The current answer in the cell
    }
    deriving (Show, Eq)

-- | The game consists of cells in 9 rows and 9 columns. 81 total cells 
--   guesses in the stack are tracked in gRecentGuesses, and the complete set 
--   of moves made so far are saved within the gNotes section.
data Game = Game
    { gBoard :: Board -- ^ The board
    , gRecentGuesses :: [((Row, Col), Int)] -- ^ Previous guesses. LIFO.
                                            -- [last guess, next guess, guess after that...]
    , gNote :: GameNotes -- ^ Notes - some for debugging. Some for the algorithm.
    }
    deriving (Show, Eq)

-- | 
data GameNotes = GameNotes 
    { gnNote :: String -- ^ Simple notes
    , gnGuessCount :: Int -- ^ Numbers of Guesses Made - for debugging
    , gnBackOutCount :: Int -- ^ Number of Guesses retracted - for debugging
    , gnRecord :: [((Row, Col), Int, String)] -- History of moves that creates this board.
    }
    deriving (Show, Eq)

type Board = Array (Row, Col) Cell -- ^ Grid of numbers

type Row = Int
type Col = Int