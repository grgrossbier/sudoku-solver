module Scratch where

import Game
import Solver
import Tools
import Text.Pretty.Simple

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