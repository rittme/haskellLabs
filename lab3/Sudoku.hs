{-
  Functional Programming -- Lab Assignment 3 : Sudoku
  ** Haskell program that can read in a Sudoku puzzle and solve it **

  Group 38 - Bernardo Rittmeyer, Modou Cissé
-}

module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Char
{- 
  #A
-}
-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [allBlankRows | x<-[1..9]]
    where allBlankRows :: [Maybe Int]
          allBlankRows = [Nothing | x<-[1..9]] 
    

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudo = isMatrixCorrect (rows sudo) && length (rows sudo) == 9

    where isMatrixCorrect :: [[Maybe Int]] -> Bool
          isMatrixCorrect = 
              foldr (\ row -> (&&) (hasSudokuValues row && length row == 9))
              True

          hasSudokuValues :: [Maybe Int] -> Bool
          hasSudokuValues row = and [True | x<-row, isValueOK x]

          isValueOK :: Maybe Int -> Bool
          isValueOK Nothing   = True
          isValueOK (Just x)  =  x > 0 || x < 10


-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudo = hasOnlyNumValues (rows sudo)

    where hasOnlyNumValues :: [[Maybe Int]] -> Bool
          hasOnlyNumValues = foldr ((&&) . areNumValues) True

          areNumValues :: [Maybe Int] -> Bool
          areNumValues rw = and [isJust x | x<-rw]

-------------------------------------------------------------------------

{- 
  #B
-}

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku row) = sequence_
                           [putStrLn $ rowToString cols | cols <- row]
  where rowToString :: [Maybe Int] -> String
        rowToString = map $ maybe '.' chr

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
           rawS <- readFile fp
           return $ validateSudoku $ Sudoku $ map stringToRow (lines rawS)
  where stringToRow :: String -> [Maybe Int]
        stringToRow = map charToMaybe

        charToMaybe :: Char -> Maybe Int
        charToMaybe n | n == '.'  = Nothing
                      | isDigit n = Just $ ord n - ord '0'
                      | otherwise = error "Invalid character in file."

        validateSudoku s | isSudoku s = s
                         | otherwise  = error "Invalid sudoku file."

-------------------------------------------------------------------------

{- 
  #C
-}
-- cell generates an arbitrary cell in a Sudoku (usually ≈ 90% Nothing)
cell :: Gen (Maybe Int)
cell = frequency[(73, return Nothing), (8, sudoNum)] where 
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
prop_Sudoku = isSudoku

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
-------------------------------------------------------------------------

{-
  #D
-}
type Block = [Maybe Int]

-- Checks if that block does not contain the same digit twice.
isOkayBlock :: Block -> Bool
isOkayBlock b = length (catMaybes b) == length (nub (catMaybes b))

-- Simple test for isOkayBlock
prop_isOkayBlock :: Bool
prop_isOkayBlock = isOkayBlock [Just 1, Just 7, Nothing,
                                Nothing, Just 3, Nothing,
                                Nothing, Nothing, Just 2] &&
                   not (isOkayBlock [Just 1, Just 7, Nothing,
                                     Just 7, Just 3, Nothing,
                                     Nothing, Nothing, Just 2])

-- Creates a list of all blocks of that Sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows                              -- rows
                     ++ [map (!! n) rows | n <- [0..8]]  -- columns
                     ++ squareBlocks rows                -- 3x3 squares
  where squareBlocks rows = [foldr ((++) . take 3 . drop x) []
                            [(!! n) rows | n <- [y .. y + 2]]
                            | x <- [0, 3, 6], y <- [0, 3, 6]]

-- Checks that all blocks do not contain the same digit twice.
isOkay :: Sudoku -> Bool
isOkay = all isOkayBlock . blocks
