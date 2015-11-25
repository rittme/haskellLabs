{-
  Functional Programming -- Lab Assignment 3 : Sudoku
  ** Haskell program that can read in a Sudoku puzzle and solve it **

  Group 38 - Bernardo Rittmeyer, Modou Cissé
-}

module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.List

{- 
  #A
-}
-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = (Sudoku [allBlankRows | x<-[1..9]])
    where allBlankRows :: [Maybe Int]
          allBlankRows = [Nothing | x<-[1..9]] 

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudo = (isMatrixCorrect (rows sudo)) && (length (rows sudo)) == 9

    where isMatrixCorrect :: [[Maybe Int]] -> Bool
          isMatrixCorrect []        = True
          isMatrixCorrect (row:xs)  = 
                        (((hasSudokuValues row) && length row == 9)
                        && (isMatrixCorrect xs))

          hasSudokuValues :: [Maybe Int] -> Bool
          hasSudokuValues row = and $ [True | x<-row, isValueOK x]

          isValueOK :: Maybe Int -> Bool
          isValueOK Nothing   = True
          isValueOK x         = ((fromMaybe' x) > 0) || ((fromMaybe' x) < 10)

          fromMaybe' :: Maybe a -> a -- to delete and use the original one ??
          fromMaybe' (Just x) = x


-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudo = (hasOnlyNumValues (rows sudo))

    where hasOnlyNumValues :: [[Maybe Int]] -> Bool
          hasOnlyNumValues []       = True
          hasOnlyNumValues (row:xs) = ((areNumValues row) && 
                                      (hasOnlyNumValues xs))

          areNumValues :: [Maybe Int] -> Bool
          areNumValues rw = and $ [not (x == Nothing) | x<-rw]


-- ******  TESTS --
example :: Sudoku
example =Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]


exampleFull :: Sudoku -- incorect
exampleFull =Sudoku
      [ [Just 3, Just 6, Just 8,Just 8,Just 7, Just 1, Just 2, Just 8,Just 8]
      , [Just 8,Just 5, Just 8,Just 8,Just 8,Just 8,Just 1, Just 8, Just 8]
      , [Just 8,Just 8,Just 9, Just 2, Just 8,Just 4, Just 7, Just 8,Just 8]
      , [Just 8,Just 8,Just 8,Just 8,Just 1, Just 3, Just 8,Just 2, Just 8]
      , [Just 4, Just 8,Just 8,Just 5, Just 8,Just 2, Just 8,Just 8,Just 9]
      , [Just 2, Just 7, Just 8,Just 4, Just 6, Just 8,Just 8,Just 8,Just 8]
      , [Just 8,Just 8,Just 5, Just 3, Just 8,Just 8, Just 9, Just 8,Just 8]
      , [Just 8,Just 8, Just 3, Just 8,Just 8,Just 8,Just 8,Just 6, Just 8]
      , [Just 8,Just 8,Just 7, Just 6, Just 9, Just 8,Just 8,Just 4, Just 3]
      ]
-------------------------------------------------------------------------

{- 
  #B
-}
-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku = undefined

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

-------------------------------------------------------------------------

{- 
  #C
-}
-- cell generates an arbitrary cell in a Sudoku (usually ≈ 90% Nothing)
cell :: Gen (Maybe Int)
cell = frequency[(73, (return Nothing)), (8, sudoNum)] where 
  sudoNum = 
    do n <- choose(1,9)
       return (Just n)


-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)


-- each generated Sudoku is actually a Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudo =  isSudoku sudo
-------------------------------------------------------------------------
