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
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudo = isMatrixCorrect (rows sudo) && length (rows sudo) == 9

    where isMatrixCorrect :: [[Maybe Int]] -> Bool
          isMatrixCorrect =
              foldr (\ row -> (&&) (hasSudokuValues row && length row == 9))
              True

          hasSudokuValues :: [Maybe Int] -> Bool
          hasSudokuValues = all isValueOK

          isValueOK :: Maybe Int -> Bool
          isValueOK Nothing   = True
          isValueOK (Just x)  =  x > 0 && x < 10


-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudo = hasOnlyNumValues (rows sudo)

    where hasOnlyNumValues :: [[Maybe Int]] -> Bool
          hasOnlyNumValues = foldr ((&&) . areNumValues) True

          areNumValues :: [Maybe Int] -> Bool
          areNumValues = all isJust

-------------------------------------------------------------------------

{-
  #B
-}

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku row) = sequence_
                           [putStrLn $ rowToString cols | cols <- row]
  where rowToString :: [Maybe Int] -> String
        rowToString = map $ maybe '.' intToDigit

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
                      | isDigit n = Just $ digitToInt n
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

-------------------------------------------------------------------------

{-
  #D
-}
type Block = [Maybe Int]

-- Checks if that block does not contain the same digit twice.
isOkayBlock :: Block -> Bool
isOkayBlock b = length (catMaybes b) == length (nub (catMaybes b))

-- Creates a list of all blocks of that Sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows                                -- rows
                     ++ transpose rows                     -- columns
                     ++ [foldr ((++) . take 3 . drop x) [] -- 3x3 blocks
                        [(!! n) rows | n <- [y .. y + 2]]
                        | x <- [0, 3, 6], y <- [0, 3, 6]]

-- Checks that all blocks do not contain the same digit twice.
isOkay :: Sudoku -> Bool
isOkay = all isOkayBlock . blocks

-------------------------------------------------------------------------

{-
  #E
-}
-- Position : coordinate that identifies a cell in the Sudoku
type Pos = (Int,Int)

-- blanks: returns positions in the given Sudoku that are still blank
blanks :: Sudoku -> [Pos]
blanks sudo = filter ( \ (x,y) -> isBlank sudo (x,y)) allSudukoPos

      where isBlank :: Sudoku -> Pos -> Bool
            isBlank sudo (row,col) =
                isNothing (rows sudo !! row !! col)

allSudukoPos :: [Pos]
allSudukoPos = [(x,y) | x <- [0..8], y <- [0..8]]

-- prop_blanks
prop_blanks :: Sudoku -> Bool
prop_blanks sudo = all
        (\(x,y) -> isNothing (rows sudo !! x !! y))
        (blanks sudo)
-- Note : we could reuse 'isBlank' function above

-- (!!=) : updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) (x:xs) (i, newVal)| i < 0 || i > length xs = error "Invalid index."
                        | i == 0                 = newVal:xs
                        | otherwise              = x:(xs !!= (i-1, newVal))

-- properties :
prop_updateListValue :: Eq a => [a] -> (Int,a) -> Bool
prop_updateListValue list (i, value) = ((list !!= (i, value)) !! i) == value

prop_updateListLength :: [a] -> (Int, a) -> Bool
prop_updateListLength list (i,x) = length list == length (list !!= (i,x))

-- update:updates the given Sudoku at the given position with the new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudo (x,y) value | x < 0 || x > 8  = error "Invalid Position."
                        | otherwise       = Sudoku newRows
          where newRows = rows sudo !!= (x, (rows sudo !! x) !!= (y,value))

-- properties : ?? to improve
prop_updateValue :: Sudoku -> Pos -> Maybe Int -> Bool
prop_updateValue sudo (x,y) value =
        (rows (update sudo (x,y) value) !! x) !! y == value


-- candidates: determines which numbers could be legally
--            written into the given blank position
candidates :: Sudoku -> Pos -> [Int]
candidates sudo (i,j) | i < 0 || i > 8  = error "Invalid Position."
                      | j < 0 || j > 8  = error "Invalid Position."
                      | otherwise       = (row `intersect` col) `intersect` box

    where full = [1..9]
          row  = full \\ catMaybes (rows sudo !! i)
          col  = full \\ catMaybes
                        ([map (!! n) (rows sudo ) | n <- [0..8]] !! j)
          box  = full \\ catMaybes ([foldr ((++) . take 3 . drop x) []
                        [(!! n) (rows sudo) | n <- [y .. y + 2]] |
                        x <- [0, 3, 6], y <- [0, 3, 6]] !! getNumBox i j)

          getNumBox :: Int -> Int -> Int
          getNumBox i j | (i >= 0 && i<=2) && (j >= 0 && j <=2) = 0
                        | (i >= 3 && i<=5) && (j >= 0 && j <=2) = 1
                        | (i >= 6 && i<=8) && (j >= 0 && j <=2) = 2
                        | (i >= 0 && i<=2) && (j >= 3 && j <=5) = 3
                        | (i >= 3 && i<=5) && (j >= 3 && j <=5) = 4
                        | (i >= 6 && i<=8) && (j >= 3 && j <=5) = 5
                        | (i >= 0 && i<=2) && (j >= 6 && j <=8) = 6
                        | (i >= 3 && i<=5) && (j >= 6 && j <=8) = 7
                        | (i >= 6 && i<=8) && (j >= 6 && j <=8) = 8


--properties : TODO

-------------------------------------------------------------------------

{-
  #F
-}

solve :: Sudoku -> Maybe Sudoku
solve sud | isSudoku sud && isOkay sud = solve' sud
          | otherwise = Nothing

solve' :: Sudoku -> Maybe Sudoku
solve' sud | null (blanks sud) = Just sud
           | otherwise = listToMaybe $ mapMaybe solve' updatedSudokus
  where firstBlank = head (blanks sud)
        firstCandidates = map Just (candidates sud firstBlank)
        updatedSudokus = map (update sud firstBlank) firstCandidates

readAndSolve :: FilePath -> IO ()
readAndSolve path = do
                      sud <- readSudoku path;
                      printResult (solve sud)
  where printResult :: Maybe Sudoku -> IO()
        printResult Nothing  = putStrLn "Unsolvable sudoku"
        printResult (Just s) = printSudoku s

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s b = isOkay s && null (blanks s) && compareExisting s b
  where compareExisting :: Sudoku -> Sudoku -> Bool
        compareExisting s r = all (sameDigit s r) $ allSudukoPos \\ blanks r

        sameDigit :: Sudoku -> Sudoku -> Pos -> Bool
        sameDigit (Sudoku s) (Sudoku d) (x,y) = (d !! x!!y) == (s !!x!!y)

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isSudoku s && isOkay s && isJust (solve s)
                    ==> fromJust (solve s) `isSolutionOf` s

fewerChecks :: Testable prop => prop -> IO ()
fewerChecks = quickCheckWith stdArgs{ maxSuccess = 30 }
